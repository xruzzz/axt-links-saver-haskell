{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveGeneric, UnicodeSyntax #-}
module Database.Queries
    (
    getDomainRows,
    getDomains
    ) where
import GHC.Generics
import qualified Data.ByteString as BS (concat, ByteString)
import qualified Data.ByteString.Char8 as BSC8 (pack, unpack, unwords)
import Database.Types (QueryDomain, PolyDomain(..))
import Database.Tables (domainsTable)
import Control.Arrow (returnA, (<<<))
import Opaleye (Column, Nullable, matchNullable, isNull,
                          Table(Table), required, queryTable,
                          Query, QueryArr, restrict, (.==), (.<=), (.&&), (.<),
                          (.===), (.++), ifThenElse, pgString, pgStrictText, aggregate, groupBy,
                          count, avg, sum, leftJoin, runQuery,
                          showSqlForPostgres, Unpackspec,PGUuid, PGInt4, PGInt8, PGText, PGDate, PGFloat8, PGBool)
import Data.Text.Internal(Text)
{-
codesQuery :: QueryCode
codesQuery = queryTable codesTable

getCode :: String -> QueryCode
getCode ss = proc () -> do
        row@(fid_domain, fid_login, fcode, fsymbols, flabel, fsymbol_labels) <- codesQuery -< ()
        restrict -< flabel .== pgString ss
        returnA -< row-}
getDomainRows :: String -> QueryDomain
getDomainRows ss = proc () ->
    do
      domain@Domain {name = pgName} <- queryTable domainsTable -< ()
      restrict -< (pgName .== (pgString ss))
      returnA -< domain

getDomains :: String -> QueryDomain
getDomains ss = proc () ->
    do
      domain <- queryTable domainsTable -< ()
      returnA -< domain
