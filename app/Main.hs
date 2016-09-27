{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import ClassyPrelude
import HTTPClient

main :: IO ()
main = do
  putStrLn "Enter the SQL query to run below."
  query <- getLine
  runQuery query
