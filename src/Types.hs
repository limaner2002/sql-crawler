{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Types where

import Data.Aeson
import Control.Lens hiding (index)
import Core

newtype Root = Root {_feed :: Feed}
  deriving Show

instance FromJSON Root where
  parseJSON (Object o) = Root <$> o .: "feed"

newtype Feed = Feed {_entries :: [Task]}
  deriving Show

instance FromJSON Feed where
  parseJSON (Object o) = Feed <$> feed
    where feed = o .: "entries"

data Task = Task
  { _content :: Content
  , _taskId :: TaskId
  }
  deriving Show

instance FromJSON Task where
  parseJSON (Object o) = do
    eTaskId <- toTaskId <$> o .: "id"
    case eTaskId of
      Left exc -> error $ show exc
      Right taskId -> 
        Task <$> o .: "content"
             <*> pure taskId

newtype Content = Content {_val :: [Text]}
  deriving Show

instance FromJSON Content where
  parseJSON (Object o) = Content <$> o .: "children"

makeLenses ''Root
makeLenses ''Feed
makeLenses ''Task
makeLenses ''Content

getTasks :: (MonadIO m, MonadThrow m, MonadBaseControl IO m) => AppianCrawler m [Task]
getTasks = do
  (SessionState mgr cookies mTok) <- get
  putStrLn "Getting assigned tasks..."
  (_, tok) <- getCSRFToken cookies
  tr <- taskReq tok
  liftIO $ print tr
  (cookies', status, body) <- join $ makeReq mgr cookies <$> taskReq tok
  put $ SessionState mgr cookies' mTok
  print status
  case decode body of
    Nothing -> return []
    Just root -> return $ root ^. feed ^. entries

filterTasks :: Text -> [Task] -> [Task]
filterTasks name = filter (\x -> name `elem` (x ^. content ^. val))
