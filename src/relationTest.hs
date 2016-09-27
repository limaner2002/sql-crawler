data SqlTable
  = Table String
  | Relation String [SqlTable]
