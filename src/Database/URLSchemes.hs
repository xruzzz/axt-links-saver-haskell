{-
    (
        save,
        UrlSchemes
    )
-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE GADTs, TypeFamilies, TemplateHaskell, QuasiQuotes, FlexibleInstances, StandaloneDeriving #-}
module Database.URLSchemes
   where
import URL
import Database.PostgreSQL.Simple
--import Database.Groundhog.TH
--import Database.Groundhog.MySQL

data UrlSchemes = UrlSchemes {
          id :: Int,
          abbreviation :: String
} deriving Show
{-
mkPersist defaultCodegenConfig [groundhog|
- entity: UrlSchemes
  dbName: url_schemes
  autoKey: null
#   constrName: URLAutoKey
  constructors:
    - name: UrlSchemes
#     keyDbName: id
      fields:
        - name: id
          type: int(255)
          dbName: id
        - name: abbreviation
          type: varchar(255)
          dbName: abbreviation
|]

data Product = Product {
  productName :: String,
  quantity :: Int,
  customer :: DefaultKey Customer
}
-}
{-

-}
save :: URL → Bool
save dd = undefined

findId :: URL → Bool
findId dd = undefined
