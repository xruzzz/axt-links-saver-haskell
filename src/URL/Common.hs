{-# LANGUAGE RecordWildCards, DeriveDataTypeable, GeneralizedNewtypeDeriving, OverloadedStrings, UnicodeSyntax #-}
module URL.Common
    (
        act,
        activeLine,
        bsPack,
        ident,
        lexeme,
        pr,
        nu,
        whitespace
    ) where

import qualified Data.ByteString as BS (concat, ByteString) 
import qualified Data.ByteString.Char8 as BSC8 (pack, unpack, unwords)
import Text.Parsec.ByteString as PSBSL (GenParser (..), Parser)
import Text.ParserCombinators.Parsec as PS( (<|>),(<?>), anyChar, char, choice, digit, letter, many, many1, manyTill, spaces, noneOf, oneOf, parse, sepBy, string, try, GenParser (..),ParseError(..)) -- as PAR
import Control.Applicative ((<$>), (<*>), (<*), (*>), (<$))
import Control.Monad (void)

act ∷ PSBSL.GenParser Char st BS.ByteString
act = do
    d ← char '#'
    return $ BSC8.pack [d]

activeLine ∷ PSBSL.GenParser Char st BS.ByteString
activeLine = (char '#' >>= return . BSC8.pack. (\x->[x]))
    <|> return nu

-- bsPack ∷ PSBSL.GenParser Char st BS.ByteString
bsPack x = x >>= return . BSC8.pack

ident ∷ PSBSL.GenParser Char st String
ident = PS.many1 (letter <|> digit <|> oneOf "_.,:(){}-@&*|/") >>= return

nu = BSC8.pack ""

pr ∷ PSBSL.GenParser Char st BS.ByteString
pr = do
        prs ← many $ oneOf "=><"
        return $ BSC8.pack prs
    <|> do
        return nu

lexeme :: Parser a -> Parser a
lexeme p = p <* whitespace

whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

