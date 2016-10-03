{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module JSONUtils where

import ClassyPrelude
import Data.Aeson

data PathPiece
  = PathIndex Int
  | PathKey Text
  deriving (Show, Eq, Read)

fromPathPiece :: PathPiece -> Text
fromPathPiece (PathIndex idx) = "[" <> tshow idx <> "]"
fromPathPiece (PathKey key) = "." <> key

showPath :: [PathPiece] -> Text
showPath [] = mempty
showPath (x:xs) = concat $ firstPiece x : fmap fromPathPiece xs
  where
    firstPiece (PathKey key) = key
    firstPiece _ = error "Can this start with an array?"

findStr :: Text -> Value -> Bool
findStr _ (Bool _) = False
findStr _ (Number _) = False
findStr _ Null = False
findStr target (String txt) = txt == target
findStr target (Object o) = any (findStr target) o
findStr target (Array a) = any (findStr target) a

getPath :: Text -> Value -> [PathPiece]
getPath _ (Bool _) = mempty
getPath _ (Number _) = mempty
getPath _ Null = mempty
getPath target (String txt)
  | isInfixOf target txt = [PathKey txt]
  | otherwise = mempty
getPath target (Object o) = foldr createPath mempty result
  where
    result = filter checkValue mValues
    checkValue Nothing = False
    checkValue (Just (_, v)) = v /= mempty
    mValues = (fmap . fmap) (\(k, v) -> (k, getPath target v)) $ fmap (\k -> inObject k o)  $ keys o
    createPath (Just (k, l)) path = PathKey k : l <> path
    createPath Nothing path = mempty
    inObject k o = case lookup k o of
      Nothing -> Nothing
      Just v -> Just (k, v)
getPath target (Array a) = f
  where
    createPath (idx, l) path = PathIndex idx : l <> path
    f = foldr createPath mempty $ filter checkValue $ zip [0..] g
    g = toList $ fmap (getPath target) a
    checkValue (_, v) = v /= mempty

traversePath :: [PathPiece] -> Value -> Maybe Value
traversePath [] _ = Nothing
traversePath _ (Bool b) = Just $ Bool b
traversePath _ (String txt) = Just $ String txt
traversePath _ (Number n) = Just $ Number n
traversePath _ Null = Just Null
traversePath [(PathKey key)] (Object o) = lookup key o
traversePath ((PathKey key):keys) (Object o) = do
  v <- lookup key o
  traversePath keys v
traversePath [(PathIndex idx)] (Array arr) = index arr idx
traversePath ((PathIndex idx):xs) (Array arr) = do
  v <- index arr idx
  traversePath xs v
