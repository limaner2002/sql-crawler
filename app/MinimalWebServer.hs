{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import           Network.HTTP.Types       (status200, status404)
import           Network.Wai              (Application, responseLBS, requestBody, requestMethod)
import           Network.Wai.Handler.Warp (run)
import ClassyPrelude
import qualified Data.ByteString.Lazy as BL
import AppianPage
import Data.Aeson

main :: IO ()
main = do
  (path:mimeType:_) <- getArgs
  run 3000 (app path $ encodeUtf8 mimeType)

app :: Text -> ByteString -> Application
app path mimeType _req sendResponse = do
  appianReq <- return . eitherDecode . fromStrict =<< requestBody _req :: IO (Either String AppianPage)
  putStrLn $ "The request is: " <> tshow appianReq
  print $ requestMethod _req

  ctnt <- readFile $ unpack path
  sendResponse $ responseLBS
    status200
    [("Content-Type", mimeType)]
    ctnt

