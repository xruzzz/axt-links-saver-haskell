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
import Debug.Trace(trace)
import Text.Parsec.ByteString as PSBSL (GenParser (..), Parser)
import Text.ParserCombinators.Parsec as PCS( (<|>),(<?>), anyChar, char, choice, digit, letter, many, many1, manyTill, skipMany, spaces, noneOf, oneOf, parse, sepBy, string, try, GenParser (..),ParseError(..))
import Text.Parsec as PS( (<|>),(<?>), anyChar, char, choice, digit, letter, many, many1, manyTill, skipMany, spaces, noneOf, oneOf, parse, sepBy, string, try,ParseError(..))
import qualified Data.ByteString as BS (ByteString, concat, putStrLn, readFile) 
import qualified Data.ByteString.Char8 as BSC8 (lines, pack, unpack,unwords)
import URL.Common(bsPack)
import Types(URL(..), Host(..))

schemeC :: PSBSL.GenParser Char st String
schemeC = many $ noneOf ":/\n"

domain :: PSBSL.GenParser Char st String
domain = many $ noneOf ":/.\n"

domains :: PSBSL.GenParser Char st [String]
domains = do
    d1 <- domain
    ds <- many (char '.' >> domain)
    return $ reverse (d1:ds)

path = many $ noneOf "?#\n"

            
pathTest = do
    p <- (PS.try $ do
        char '/'
        pp <- path
        return $ Just pp
        <|> return Nothing)
    q <- (PS.try $ do
        char '?'
        qq <- many (noneOf "#")
        return $ Just qq
        <|> return Nothing)
    f <- (PS.try $ do
        char '#'
        ff <- many (noneOf "\n")
        return $ Just ff
        <|> return Nothing)
    return (p, q, f)

url ::  PSBSL.GenParser Char st URL
url = do
    sch <- schemeC
    string "://"
    w <- ((PS.try (string "www.") >> return True) <|> return False)
    ds <- domains
    (p,q,f) <- pathTest
    many anyChar
    return $ URL {_scheme = BSC8.pack sch, _auth = Nothing, _host = Host {_www=w, _domains = fmap BSC8.pack ds}, _port= Nothing, _path =BSC8.pack <$> p, _c_query = BSC8.pack <$> q, _fragment = BSC8.pack <$> f}

parseURL ∷ String -> Either PS.ParseError URL
parseURL = parse url "(unknown parseLine)" . BSC8.pack
