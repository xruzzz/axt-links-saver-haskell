{-
    (
        save,
        UrlSchemes
    )
-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE GADTs, TypeFamilies, TemplateHaskell, QuasiQuotes, FlexibleInstances, StandaloneDeriving #-}
module Database.Categories
   where
import URL
-- import Database.Groundhog.TH
-- import Database.Groundhog.MySQL

data Categories = Categories {
          id :: Int,
          label :: String
} deriving Show
{-
mkPersist defaultCodegenConfig [groundhog|
- entity: Categories
  dbName: url_schemes
  autoKey: null
#   constrName: URLAutoKey
  constructors:
    - name: Categories
#     keyDbName: id
      fields:
        - name: id
          type: int(11)
          dbName: id
        - name: label
          type: varchar(255)
          dbName: label
|]

-}