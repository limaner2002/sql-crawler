{-# LANGUAGE NoImplicitPrelude #-}

module Relations where

import ClassyPrelude

data SqlTable
  = Table String
  | Relation String [SqlTable]
