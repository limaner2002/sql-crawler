{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- Possibly change the b to IO
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Core
  ( crawlAppian
  , crawlSql
  , runQuery
  , module ClassyPrelude
  , module Control.Monad.Base
  , module Control.Monad.Trans.Control
  , SessionState (..)
  , Control.Monad.State.Lazy.get
  , Control.Monad.State.Lazy.put
  , getCSRFToken
  , taskReq
  , makeReq
  , AppianCrawler
  ) where

import ClassyPrelude
import Control.Monad.State.Lazy hiding (mapM_)
import Network.HTTP.Client
import Network.HTTP.Conduit
import Network.HTTP.Simple
import Control.Monad.Trans.Resource
import Control.Monad.Trans.Control
import Control.Monad.Trans.Class
import Control.Monad.Base
import Data.Conduit
import qualified Data.Conduit.Combinators as CC
import Network.HTTP.Types.Header
import Network.HTTP.Types.Status
import Control.Arrow.ArrowList
import Control.Arrow
import Control.Arrow.ListArrow
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy.Search
import Text.XML.HXT.Core

import Data.Aeson

-- domain = "portal-dev.usac.org"
domain = "portal-test.usac.org"
protocol = "https"
baseURL = protocol <> "://" <> domain

authReq :: MonadThrow m => [(ByteString, Maybe ByteString)] -> m Request
authReq params = setRequestMethod "POST"
  <$> addRequestHeader "Content-Type" "application/x-www-form-urlencoded"
  <$> setQueryString (params <> authParams)
  <$> setRequestHeaders baseHeaders
  <$> parseUrlThrow (baseURL <> "/suite/auth?appian_environment=tempo")

dbReq :: MonadThrow m => m Request
dbReq = setRequestHeaders baseHeaders
  <$> parseUrlThrow (baseURL <> "/database/index.php")

suiteReq :: MonadThrow m => m Request
suiteReq = setRequestHeaders baseHeaders
  <$> parseUrlThrow (baseURL <> "/suite")

newtype TaskId = TaskId String
  deriving (Show, Read, Eq, Monoid)

newtype AppianCrawler m a = AppianCrawler
  { runAppian :: StateT SessionState m a }
  deriving (Functor, Applicative, Monad, MonadThrow, MonadIO, MonadTrans, MonadState SessionState)

instance MonadBase b m => MonadBase b (AppianCrawler m) where
  liftBase = lift . liftBase

instance MonadBaseControl b m => MonadBaseControl b (AppianCrawler m) where
  type StM (AppianCrawler b) a = StM (StateT SessionState b) a
  liftBaseWith f = AppianCrawler $ liftBaseWith $ \rib -> f (rib . runAppian)
  restoreM = AppianCrawler . restoreM

newtype SqlCrawler m a = SqlCrawler
  { runCrawler :: AppianCrawler m a}
  deriving (Functor, Applicative, Monad, MonadThrow, MonadIO, MonadTrans, MonadState SessionState)

newtype SqlToken = SqlToken (ByteString, Maybe ByteString)

data SessionState = SessionState
  { mgr :: Manager
  , cookies :: CookieJar
  , sqlToken :: Maybe SqlToken
  }

instance MonadBase b m => MonadBase b (SqlCrawler m) where
  liftBase = lift . liftBase

instance MonadBaseControl b m => MonadBaseControl b (SqlCrawler m) where
  type StM (SqlCrawler b) a = StM (StateT SessionState b) a
  liftBaseWith f = SqlCrawler $ liftBaseWith $ \rib -> f (rib . runCrawler)
  restoreM = SqlCrawler . restoreM

crawlAppian :: MonadBaseControl IO m => AppianCrawler m a -> m a
crawlAppian crawler = bracket alloc free go
  where
    alloc = do
      mgr <- liftBase $ do
        putStrLn "Logging in now"
        newManager tlsManagerSettings
      (cookies, status, tok) <- liftBase $ login mgr
      liftBase $ print status
      return $ SessionState mgr cookies tok
    free (SessionState mgr cookies _) = do
      liftBase $ do
        putStrLn "Logging out..."
      (_, status, _) <- liftBase $ join $ makeReq mgr cookies <$> logoutReq
      liftBase $ print status
    go session = evalStateT (runAppian crawler) session

crawlSql :: (MonadIO m, MonadThrow m, MonadBaseControl IO m) => SqlCrawler m a -> AppianCrawler m a
crawlSql (SqlCrawler appianCrawler) = do
  dbLogin
  appianCrawler

makeReq
  :: (MonadIO m, MonadThrow m, MonadBaseControl IO m) =>
     Manager
     -> CookieJar -> Request -> m (CookieJar, Status, BL.ByteString)
makeReq mgr cookies req = do
  now <- liftBase getCurrentTime
  let (req', _) = insertCookiesIntoRequest req cookies now

  makeReq' mgr req'

makeReq'
  :: (MonadIO m, MonadThrow m, MonadBaseControl IO m) =>
     Manager
     -> Request
     -> m (CookieJar, Status, BL.ByteString)
makeReq' mgr req = do
  runResourceT $ do
    resp <- http req mgr
    body <- responseBody resp $$+- CC.sinkLazy
    return $ (responseCookieJar resp, responseStatus resp, body)

login :: (MonadIO m, MonadThrow m, MonadBaseControl IO m) => Manager -> m (CookieJar, Status, Maybe SqlToken)
login mgr = do
  (cookies, status, _) <- join $ makeReq mgr mempty <$> suiteReq
  print status
  req <- authReq (fmap (\(name, value) -> (name, Just value)) $ getCSRFToken cookies)

  print req

  (cookies', status, body) <- makeReq mgr cookies req
  print status

  return (cookies', status, Nothing)

dbLogin :: (MonadIO m, MonadThrow m, MonadBaseControl IO m) => AppianCrawler m ()
dbLogin = do
  (SessionState mgr cookies mTok) <- get
  (cookies', status', body') <- join $ makeReq mgr cookies <$> dbReq
  print status'

  let sqlToken = BL.toStrict <$> getToken body'
      wrapTok = (SqlToken . (,) "token" . Just)

  case wrapTok <$> sqlToken of
    Nothing -> throwM $ RequestException "Could not find a token!" cookies status' body'
    mTok -> put $ SessionState mgr cookies' mTok

baseHeaders :: [(HeaderName, ByteString)]
baseHeaders = [ ("Accept-Language", "en-US,en;q=0.5")
              , ("Upgrade-Insecure-Requests", "1")
              , ("Accept-Encoding", "gzip, deflate, br")
              , ("User-Agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_6) AppleWebKit/602.1.50 (KHTML, like Gecko) Version/10.0 Safari/602.1.50")
              , ("Accept", "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8")
              ]

authParams :: [(ByteString, Maybe ByteString)]
authParams = [ ("un", Just "app1_sd1_full1@mailinator.com")
           , ("pw", Just "USACuser123$")
           , ("spring_security_remember_me", Just "on")
           ]

getCSRFToken :: MonadThrow m => CookieJar -> m (ByteString, ByteString)
getCSRFToken cookies = do
  let r = runLA (arrL destroyCookieJar >>> cookieHasName "__appianCsrfToken" >>> constA "X-APPIAN-CSRF-TOKEN" &&& arr (cookie_value))

  case r cookies of
    [t] -> return t
    _ -> throwM $ NoTokenException "There is not CSRF token present!"

cookieHasName :: ArrowList a => ByteString -> a Cookie Cookie
cookieHasName str = isA (\c -> cookie_name c == str)

runQuery :: (MonadIO m, MonadThrow m, MonadBaseControl IO m) => ByteString -> SqlCrawler m BL.ByteString
runQuery query = do
  (SessionState mgr cookies mTok) <- get
  putStrLn "Running query..."

  (cookies', status, body) <- case mTok of
    Nothing -> throwM $ NoTokenException "There is no phpAdmin token!"
    Just tok -> join $ makeReq mgr (addSqlCookie cookies) <$> sqlReq tok query
  
  put (SessionState mgr cookies' mTok)
  print status
  return body
  
logoutReq :: MonadThrow m => m Request
logoutReq = setRequestHeaders baseHeaders
  <$> parseUrlThrow (baseURL <> "/suite/logout")

sqlReq :: MonadThrow m => SqlToken -> ByteString -> m Request
sqlReq (SqlToken param) query = addRequestHeader "X-Requested-With" "XMLHttpRequest"
     <$> addRequestHeader "Content-Type" "application/x-www-form-urlencoded"
     <$> setRequestHeaders baseHeaders
     <$> setQueryString (param : sqlParams query)
     <$> setRequestMethod "POST"
     <$> parseUrlThrow (baseURL <> "/database/sql.php")

sqlParams :: ByteString -> [(ByteString, Maybe ByteString)]
sqlParams query = [ ("db", Just "information_schema")
                  , ("table", Just "")
                  , ("sql_query", Just query)
                  , ("goto", Just "server_sql.php")
                  , ("display_options_form", Just "1")
                  , ("pftext", Just "F")
                  , ("display_binary", Just "on")
                  , ("geoOption", Just "GEOM")
                  , ("ajax_request", Just "true")
                  , ("_nocache", Just "147489670451485973")
                  ]

addSqlCookie :: CookieJar -> CookieJar
addSqlCookie cookies = insertCheckedCookie savedSql cookies True
  where
    savedSql = Cookie "pmaCookieVer" "4" expTime domain path createTime lastAccess persistent hostOnly secureOnly httpOnly
    (Cookie _ _ expTime domain path createTime lastAccess persistent hostOnly secureOnly httpOnly)
      = (\x -> indexEx x 0) $ runLA (arrL destroyCookieJar >>> cookieHasName "mySSO") cookies

data NoTokenException = NoTokenException Text

instance Show NoTokenException where
  show (NoTokenException msg) = "NoTokenException: " <> show msg

instance Exception NoTokenException

data RequestException = RequestException
  { excMessage :: Text
  , excCookies :: CookieJar
  , excStatus :: Status
  , excBody :: BL.ByteString
  }

instance Show RequestException where
  show (RequestException msg _ _ _) = "RequestException: " <> show msg

instance Exception RequestException

getToken :: BL.ByteString -> Maybe BL.ByteString
getToken body = join $ index <$> firstPart <*> pure 0
  where
    firstPart = splitKeepFront "&" <$> index (splitKeepEnd "token=" body) 1

getResultBody :: MonadThrow m => BL.ByteString -> m String
getResultBody body = do
  case decode body of
    Nothing -> throwM $ ParseJSONException "Could not parse the JSON." body
    Just (SqlResult r) -> return r

getResult :: BL.ByteString -> IO (Either SomeException [[String]])
getResult body = sequence $ (\r -> runX $ parseResult r) <$> getResultBody body

readResult :: String -> IOSLA (XIOState s) a XmlTree
readResult resp = constA resp >>> readFromString [withValidate no, withParseHTML yes, withWarnings no]

parseResult :: String -> IOSLA (XIOState s) a [String]
parseResult resp = readResult resp
  //> hasAttrValue "class" (isPrefixOf "table_results")
  >>> getChildren >>> getChildren
  >>> (arr $ runLA $ getChildren //> getText)

newtype SqlResult
  = SqlResult String
  deriving (Show)

instance FromJSON SqlResult where
  parseJSON (Object o) = SqlResult <$> o .: "message"

data KeyColumnUsage = KeyColumnUsage
  { constraintCatalog :: String
  , constraintSchema :: String
  , constraintName :: String
  , tableCatalog :: String
  , tableSchema :: String
  , tableName :: String
  , columnName :: String
  , ordinalPosition :: Int
  , positionInUniqueConstraint :: Int
  , referencedTableSchema :: String
  , referencedTableName :: String
  , referencedColumnName :: String
  } deriving Show

data InvalidColumnData = InvalidColumnData Text

instance Show InvalidColumnData where
  show (InvalidColumnData msg) = "InvalidColumnData: " <> show msg

instance Exception InvalidColumnData

buildKeyColumnUsage :: MonadThrow m => [String] -> m KeyColumnUsage
buildKeyColumnUsage [cc, cs, cn, tc, ts, tn, coln, ordpos, posunique, rts, rtn, rcn] =
  case mColumnUsage of
    Nothing -> throwM $ InvalidColumnData "Could not create KeyColumnUsage using the data returned from the database."
    Just columnUsage -> return columnUsage
  where
    mColumnUsage = KeyColumnUsage cc cs cn tc ts tn coln
       <$> readMay ordpos
       <*> readMay posunique
       <*> pure rts
       <*> pure rtn
       <*> pure rcn
buildKeyColumnUsage _ = throwM $ InvalidColumnData "Incorrect number of fields for KeyColumnUsage."

keyColumnUsage :: [String] -> Either SomeException KeyColumnUsage
keyColumnUsage = buildKeyColumnUsage

getTaskStatus :: (MonadIO m, MonadThrow m, MonadBaseControl IO m) => TaskId -> AppianCrawler m BL.ByteString
getTaskStatus taskId = do
  (SessionState mgr cookies mTok) <- get
  putStrLn "Getting task status..."
  (_, tok) <- getCSRFToken cookies
  (cookies', status, body) <- join $ makeReq mgr cookies <$> statusReq taskId tok
  print status
  put $ SessionState mgr cookies' mTok
  return body

statusReq :: MonadThrow m => TaskId -> ByteString -> m Request
statusReq (TaskId taskId) csrfToken = setQueryString taskParams
  <$> setRequestMethod "POST"
  <$> setRequestBody "accepted"
  <$> setRequestHeader "Accept" ["application/vnd.appian.tv.ui+json"]
  <$> setRequestHeader "X-HTTP-Method-Override" ["PUT"]
  <$> appianReq (("X-APPIAN-CSRF-TOKEN", csrfToken) : baseHeaders) ("/suite/rest/a/task/latest/" <> taskId <> "/status")

taskReq :: MonadThrow m => ByteString -> m Request
taskReq csrfToken = setQueryString taskParams
  <$> appianReq (("X-APPIAN-CSRF-TOKEN", csrfToken) : baseHeaders <> taskHeaders) "/suite/api/feed/tempo"

taskHeaders :: [(HeaderName, ByteString)]
taskHeaders = [ ("Referer", "https://portal-test.usac.org/suite/tempo/news/all")
              , ("x-appian-suppress-www-authenticate", "true")
              , ("Accept", "application/atom+json,application/json")
              , ("X-Appian-Ui-State", "stateful")
              , ("X-Atom-Content-Type", "application/html")
              ]

taskParams :: [(ByteString, Maybe ByteString)]
taskParams = [ ("m", Just "menu-tasks")
             , ("t", Just "t")
             , ("s", Just "pt")
             , ("defaultFacets", Just "%5Bstatus-open%5D")
             ]
  
-- https://portal-test.usac.org/suite/tempo/tasks/assignedtome

appianReq :: MonadThrow m => [(HeaderName, ByteString)] -> String -> m Request
appianReq headers path = setRequestHeaders headers
  <$> parseUrlThrow (baseURL <> path)

data ParseJSONException = ParseJSONException Text BL.ByteString

instance Show ParseJSONException where
  show (ParseJSONException msg _) = "ParseJSONException: " <> show msg

instance Exception ParseJSONException
