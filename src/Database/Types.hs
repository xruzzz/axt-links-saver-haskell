{-# LANGUAGE Arrows, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, TemplateHaskell, UnicodeSyntax #-}
module Database.Types
    (
        Domain,
        DomainColumns,
        DomainPGR,
        DomainPGW,
        PolyDomain(..),
        QueryDomain
    ) where
import GHC.Generics
import Control.Arrow
import qualified Data.ByteString as BS (concat, ByteString)
import qualified Data.ByteString.Char8 as BSC8 (pack, unpack, unwords)
import Data.Profunctor.Product (p2, p3, p6)
import Data.Profunctor.Product.Default (Default(..))

import Opaleye (Column, Nullable, PGUuid, PGInt4, PGInt8, PGText, PGDate, PGFloat8, PGBool, Query, QueryArr, QueryRunner, QueryRunnerColumnDefault(..), Table(Table), Unpackspec,
                            fieldQueryRunnerColumn, matchNullable, isNull,
                            required, queryTable,
                            restrict, (.==), (.<=), (.&&), (.<),
                            (.===), (.++), ifThenElse, pgString, aggregate, groupBy,
                            count, avg, sum, leftJoin, runQuery, showSqlForPostgres)

import Database.PostgreSQL.Simple.FromField (FromField(..))
                            
newtype BaseId = BaseId Int deriving (Show)

instance FromField BaseId where
  fromField field bs = BaseId <$> fromField field bs

instance QueryRunnerColumnDefault PGInt4 BaseId where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

type DomainColumns = ( Column PGInt4, Column PGText)
type QueryDomain = Query DomainPGR
data PolyDomain lvl nm = Domain {level ∷ lvl, name ∷ nm}
    deriving (Show)

type Domain = PolyDomain Int String
type DomainPGW = PolyDomain (Column PGInt4) (Column PGText)
type DomainPGR = DomainPGW

