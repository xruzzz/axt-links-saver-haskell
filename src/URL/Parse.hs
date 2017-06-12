{-
semigroups
 Можно взять готовый парсинг
 Network.URI
-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE MonadComprehensions #-}
module URL.Parse
    (
      parseURL
    ) where

import Network.URI (URI)
import Data.Word
import Data.List.Split
import Text.Parsec.ByteString as PSBSL (GenParser (..), Parser)
import Text.ParserCombinators.Parsec as PS( (<|>),(<?>), anyChar, char, choice, digit, letter, many, many1, manyTill, spaces, noneOf, oneOf, parse, sepBy, string, try, GenParser (..),ParseError(..))
import qualified Data.ByteString as BS (ByteString, concat, putStrLn, readFile) 
import qualified Data.ByteString.Char8 as BSC8 (lines, pack, unpack,unwords)
import URL.Common(bsPack)
import Types(URL(..))

schemeC :: PSBSL.GenParser Char st BS.ByteString
schemeC = do
    sr <- many $ noneOf ":/\n"
    return $ BSC8.pack sr

--    return $ BSC8.pack sr

-- firstDomain :: PSBSL.GenParser Char st BS.ByteString
domain :: PSBSL.GenParser Char st BS.ByteString
domain = do
    sr <- many $ noneOf ":/.\n"
    return $ BSC8.pack sr

{-
domain :: PSBSL.GenParser Char st BS.ByteString
domain = do
    string "."
    sr ← many $ noneOf ":/.\n"
    many anyChar
    return $ BSC8.pack sr
    -}
-- domains :: PSBSL.GenParser Char st [BS.ByteString]
domains = do
    d1 <- domain
    ds <- many domain
    return $ reverse (d1:ds)

url ::  PSBSL.GenParser Char st URL
url = do
    sch <- schemeC
    string "://"
--    domains
--    string "/"
    many anyChar
    return URL {_scheme = sch }

parseURL ∷ String -> Either PS.ParseError URL
parseURL = parse url "(unknown parseLine)" . BSC8.pack
