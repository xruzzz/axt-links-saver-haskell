{-
semigroups
 Можно взять готовый парсинг
 Network.URI
-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE TemplateHaskell #-}
module Types
    (
      URL(..)
    ) where
import Network.URI (URI)
--import Data.List.NonEmpty   TODO 2 Использовать в Host Domains не пустой список
import Data.Word
import Data.List.Split
import qualified Data.ByteString as BS (ByteString, concat, putStrLn, readFile) 
import qualified Data.ByteString.Char8 as BSC8 (lines, pack, unpack,unwords)
import Control.Lens

data Port = Port Int
                deriving (Eq, Show, Read)

data URIAuth1 = URIAuth1 {
     _login::BS.ByteString,
     _password::Maybe BS.ByteString
    } deriving (Eq, Show, Read)

data Host = Domains [BS.ByteString] | IPv4 Word32
                    deriving (Eq, Show, Read)

data URL = URL {
                    _scheme::BS.ByteString,
                    _auth::Maybe URIAuth1,
                    _host::Host,
                    _port::Maybe Port,
                    _path::Maybe BS.ByteString,
                    _c_query::Maybe BS.ByteString,
                    _fragment::Maybe BS.ByteString
                } deriving (Eq, Show, Read)
makeLenses ''URIAuth1
makeLenses ''URL
                {-
instance Show URL where
    show (URL sch usr psw dom po pa) = sch ++"://"++ foldl1 (++) dom
-}
instance Bounded Port where
    minBound = Port 0
    maxBound = Port (2^16)

