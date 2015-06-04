{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE GADTs, TypeFamilies, TemplateHaskell, QuasiQuotes, FlexibleInstances, StandaloneDeriving #-}
{-
cabal sandbox add-source
-}
module Main where
import Data.Char
import Prelude.Unicode
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Text.Regex.PCRE
import Control.Monad
import Database.Groundhog.TH
import Database.Groundhog.MySQL
import Data.IORef
import Data.Time.Clock.POSIX


data UrlSchemes = UrlSchemes {
	idn :: Int,
	abbreviation :: String
} deriving Show



mkPersist defaultCodegenConfig [groundhog|
- entity: UrlSchemes               # Name of the datatype
  constructors:
    - name: UrlSchemes
      fields:
        - name: idn
          type: integer
          dbName: id
        - name: abbreviation
          type: varchar(255)
          dbName: abbreviation
|]





genCmd :: String → [[String]]
genCmd inUrl =
	let	regexp = "^([a-z]+)://([a-z]+)?.?([a-z]+)?.([a-z]+)/?(.+)?"
		match = inUrl =~ regexp :: [[String]]
	in
		match

getConInf :: ConnectInfo
getConInf = defaultConnectInfo {connectPassword = "***", connectDatabase = "axdb_local"}

main::IO()
main = withMySQLConn getConInf $ runDbConn $ do
		insert $ UrlSchemes {idn = 2, abbreviation = "http"}
