{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

import Data.Time
import Control.Monad.Trans.Resource
import Control.Monad.Trans.Reader
import ClassyPrelude
import Network.HTTP.Client
import Network.HTTP.Conduit
import Data.Conduit
import qualified Data.Conduit.Combinators as CC
import Network.HTTP.Simple
import qualified Data.ByteString.Lazy as BL
import Network.HTTP.Types.Header
import Network.HTTP.Types.Status
import Data.Attoparsec.ByteString.Lazy
import Data.ByteString.Lazy.Search
import Text.XML.HXT.Core

import Data.Aeson

import Control.Arrow.ArrowList
import Control.Arrow
import Control.Arrow.ListArrow

domain = "portal-dev.usac.org"
protocol = "https"
baseURL = protocol <> "://" <> domain

suiteReq :: MonadThrow m => m Request
suiteReq = parseUrlThrow $ baseURL <> "/suite"

authReq :: MonadThrow m => m Request
authReq = setRequestMethod "POST" <$> parseUrlThrow (baseURL <> "/suite/auth?appian_environment=tempo")

logoutReq :: MonadThrow m => m Request
logoutReq = parseUrlThrow $ baseURL <> "/suite/logout"

baseHeaders :: [(HeaderName, ByteString)]
baseHeaders = [ ("Accept-Language", "en-US,en;q=0.5")
              , ("Upgrade-Insecure-Requests", "1")
              , ("Accept-Encoding", "gzip, deflate, br")
              , ("User-Agent", "Mozilla/5.0 (Windows NT 6.1; WOW64; rv:48.0) Gecko/20100101 Firefox/48.0")
              , ("Accept", "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8")
              ]

authParams :: [(ByteString, Maybe ByteString)]
authParams = [ ("un", Just "joshua.mccartney@itgfirm.com")
           , ("pw", Just "Booy'et7")
           , ("spring_security_remember_me", Just "on")
           ]

serverSqlReq :: MonadThrow m => (ByteString, Maybe ByteString) -> m Request
serverSqlReq param = addRequestHeader "Referer" "https://portal-dev.usac.org/database/index.php"
  <$> setRequestHeaders baseHeaders
  <$> setQueryString (param : serverSqlParams)
  <$> parseUrlThrow (baseURL <> "/database/server_sql.php")

serverSqlParams :: [(ByteString, Maybe ByteString)]
serverSqlParams = [ ("db", Nothing)
--                   , ("token", Just "424db9868176e6bc139e33b2dae21161")
                  , ("ajax_request", Just "true")
                  , ("ajax_page_request", Just "true")
                  , ("_nocache", Just "147465964388341226")
                  ]

dbReq :: MonadThrow m => m Request
dbReq = parseUrlThrow $ baseURL <> "/database/index.php"

sqlReq :: MonadThrow m => (ByteString, Maybe ByteString) -> ByteString -> m Request
sqlReq param query = addRequestHeader "X-Requested-With" "XMLHttpRequest"
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

prepareReq :: CookieJar -> [(HeaderName, ByteString)] -> IO Request -> IO Request
prepareReq cookies headers ioReq = do
  now <- getCurrentTime
  (req, _) <- (\req -> insertCookiesIntoRequest req cookies now) <$> setRequestHeaders headers <$> ioReq
  return req

makeReq :: Manager -> CookieJar -> [(HeaderName, ByteString)] ->  IO Request -> IO (CookieJar, BL.ByteString)
makeReq mgr cookies headers ioReq = do
  now <- getCurrentTime
  (req, _) <- (\req -> insertCookiesIntoRequest req cookies now) <$> setRequestHeaders headers <$> ioReq

  (cookies, status, body) <- makeReq' mgr req

  print status
  return (cookies, body)

makeReq' :: Manager -> Request -> IO (CookieJar, Status, BL.ByteString)
makeReq' mgr req = do
  runResourceT $ do
    resp <- http req mgr
    body <- responseBody resp $$+- CC.sinkLazy
    return $ (responseCookieJar resp, responseStatus resp, body)

getCSRFToken = runLA (arrL destroyCookieJar >>> cookieHasName "__appianCsrfToken" >>> constA "X-APPIAN-CSRF-TOKEN" &&& arr (Just . cookie_value))

cookieHasName :: ArrowList a => ByteString -> a Cookie Cookie
cookieHasName str = isA (\c -> cookie_name c == str)

getToken :: BL.ByteString -> Maybe BL.ByteString
getToken body = join $ index <$> firstPart <*> pure 0
  where
    firstPart = splitKeepFront "&" <$> index (splitKeepEnd "token=" body) 1

readResult :: String -> IOSLA (XIOState s) a XmlTree
readResult resp = constA resp >>> readFromString [withValidate no, withParseHTML yes, withWarnings no]

parseResult :: String -> IOSLA (XIOState s) a [String]
parseResult resp = readResult resp
  //> hasAttrValue "class" (isPrefixOf "table_results")
  >>> getChildren >>> getChildren
  >>> (arr $ runLA $ getChildren //> getText)

doIt :: ByteString -> IO ([CookieJar], [BL.ByteString])
doIt query = do
  mgr <- newManager tlsManagerSettings
  (cookies, body) <- makeReq mgr mempty baseHeaders dbReq -- get CSRF token

  let req = prepareReq cookies baseHeaders (setQueryString (getCSRFToken cookies <> authParams) <$> authReq)

  (cookies', status, body') <- join $ makeReq' mgr <$> addRequestHeader "Content-Type" "application/x-www-form-urlencoded" <$> req -- Login
  print status

  (cookies'', body'') <- makeReq mgr cookies' baseHeaders dbReq -- DB Login and get SSO token

  let req = sqlReq ("token", mTok) query
      mTok = BL.toStrict <$> getToken body''

  print =<< req

  (cookies''', body''') <- makeReq mgr (addSqlCookie cookies'') mempty req

  (cookies'''', body'''') <- makeReq mgr cookies' baseHeaders logoutReq

  return ([cookies, cookies', cookies'', cookies'''], [body, body', body'', body'''])
 
addSqlCookie :: CookieJar -> CookieJar
addSqlCookie cookies = insertCheckedCookie savedSql cookies True
  where
    savedSql = Cookie "pmaCookieVer" "4" expTime domain path createTime lastAccess persistent hostOnly secureOnly httpOnly
    (Cookie _ _ expTime domain path createTime lastAccess persistent hostOnly secureOnly httpOnly)
      = (\x -> indexEx x 0) $ runLA (arrL destroyCookieJar >>> cookieHasName "mySSO") cookies

getResultBody :: BL.ByteString -> Maybe String
getResultBody body = do
  (SqlResult r) <- decode body
  return r

getResult :: BL.ByteString -> IO (Maybe [[String]])
getResult body = sequence $ (\r -> runX $ parseResult r) <$> getResultBody body

runQuery :: ByteString -> IO ()
runQuery query = do
  (_, body) <- doIt query
  r <- join <$> (sequence $ getResult <$> body `index` 3)
  (mapM_ . mapM_) print $ filter (/= mempty) <$> r

newtype SqlResult
  = SqlResult String
  deriving (Show)

instance FromJSON SqlResult where
  parseJSON (Object o) = SqlResult <$> o .: "message"
