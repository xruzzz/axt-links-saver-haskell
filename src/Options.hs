{-# LANGUAGE UnicodeSyntax #-}
module Options
    (
        Options(..),
        cases,
        defaultOptions,
        options
    ) where

import qualified Data.ByteString as BS (concat, ByteString)
import qualified Data.ByteString.Char8 as BSC8 (pack, unpack, unwords)
import System.Console.GetOpt(ArgDescr(..), ArgOrder(..), OptDescr(..), getOpt)

data Options = Options  {
    optRequest ∷ String,
    optInput  ∷ IO String,
    optOutput ∷ String → IO (),
    optUserInterface ∷ String
  }


defaultOptions ∷ Options
defaultOptions = Options {
    optRequest = "GetLink", 
    optInput  = getContents,
    optOutput = putStr,
    optUserInterface = "cui"
  }

{-
    При использовании должен быть преобразован в
    options ∷ [OptDescr (Options → IO Options)]
    -}
options ∷ [ArgDescr a0 -> String -> OptDescr a0]
options = [
    Option ['c'] ["case"        ],
    Option ['h'] ["help"        ],
    Option ['i'] ["input"       ],
    Option ['o'] ["output"      ],
    Option ['u'] ["interface"   ],
    Option ['v'] ["version"     ]
  ]

-- Сценарии главного варианта использования
cases = ["GetLink"]
