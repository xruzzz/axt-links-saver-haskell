{-
    (
        save,
        UrlSchemes
    )
-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE GADTs, TypeFamilies, TemplateHaskell, QuasiQuotes, FlexibleInstances, StandaloneDeriving #-}
module Database.Domains
   where
import URL
import Database.PostgreSQL.Simple

data UrlSchemes = UrlSchemes {
          id :: Int,
          abbreviation :: String
} deriving Show

save :: URL → Bool
save dd = undefined

findId :: URL → Bool
findId dd = undefined
