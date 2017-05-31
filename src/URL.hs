{-
semigroups
 Можно взять готовый парсинг
 Network.URI
-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE MonadComprehensions #-}
module URL
    (
      parseURL,
      URL(..)
    ) where
import Text.Regex.PCRE
import Network.URI (URI)
--import Data.List.NonEmpty   TODO 2 Использовать в Host Domains не пустой список
import Data.Word
import Data.List.Split

data Port = Port Int
                deriving (Eq, Show, Read)

data URIAuth1 = URIAuth1 {
     login::String,
     password::Maybe String
    } deriving (Eq, Show, Read)
{-
data URI = URI {
                    scheme:: String,
                    authority:: String,
                    path::Maybe String
                    query::Maybe String
                    fragment::Maybe String
                } deriving Show

data FQDN = FQDN
               {
               }-}

data Host = Domains [String] | IPv4 Word32
                    deriving (Eq, Show, Read)

data URL = URL {
                    scheme::String,
                    auth:: Maybe URIAuth1,
                    host::Host,
                    port::Maybe Port,
                    path::Maybe String,
                    c_query::Maybe String,
                    fragment::Maybe String
                } deriving (Eq, Show, Read)
{-
instance Show URL where
    show (URL sch usr psw dom po pa) = sch ++"://"++ foldl1 (++) dom
-}
instance Bounded Port where
    minBound = Port 0
    maxBound = Port (2^16)
{-
    https://ru.wikipedia.org/wiki/URI
    https://tools.ietf.org/html/rfc3986
-}

--(ʃ) :: Bool → a → Maybe a
--(ʃ) pr s = if pr then Just s else Nothing
-- ♎
fas :: [String] → Int → a→ Maybe a
fas l p s = if length (l !! p) > 0 then Just s else Nothing

fasS :: [String] → Int → Maybe String
fasS l p = if length (l !! p) > 0 then Just (l !! p) else Nothing

-- [s |length (l !! p) > 0]::Maybe a

parseURL :: String → Maybe URL
parseURL inUrl =
    let regexp = "^(([^:/?#]+):)?(//(([^/?#:]{1,10000})(:([^/?#]{1,10000}))?@)?([^/?#:]*)(:(\\d{1,5}))?)?(/([^?#]*))?(\\?([^#]*))?(#(.*))?"
        regexpDom = "^(([^/?#:]{1,10000})(:([^/?#]{1,10000}))?@)?([\\w-.:@]{1,10000}):?(\\d{1,5})$"
        patFQDNPort = "^([^/?#]{1,10000}@)?([^/?#:@]{1,10000})"
        posScheme = 2
        posLogin = 5
        posPassword = 7
        posDomains = 8
        posPort = 10
        posPath = 12
        posQuery = 14
        posFragment = 16
        posDomainsToSource = 2
    -- ^(\\w{1,255})://([\\w-.:@]{1,10000})/?(.{1,10000})?"
        match1 = foldl1 (++) (inUrl =~ regexp :: [[String]])
        foldLengs = foldl (\acc x → acc && if x > 0 then True else False) True
    in
        if foldLengs ([length (match1 !! x)| x←[posScheme, posDomains]]) then
                        Just URL {
                            scheme = match1 !! posScheme,
                            auth = fas match1 posLogin URIAuth1 {login= match1 !! posLogin, password = fasS match1 posPassword },
                            host = Domains match1, -- . reverse $ splitOn "." (match1 !! posDomains),
                            port = fas match1 posPort (Port (read (match1 !! posPort)::Int)),-- [(Port (read (match1 !! posPort)::Int)) |length (match1 !! posPort) > 0]::Maybe (Port a),
                            path = fasS match1 posPath,
                            c_query = fasS match1 posQuery,
                            fragment = fasS match1 posFragment}
        else
            Nothing

{-
http://learnyouahaskell.com/input-and-output
ftp://login:pass@serv.example.com:21/function/reg.php;type=i;
http://hackage.haskell.org/package/network-2.1.0.0/docs/Network-URI.html#v%3AURI
http://www.ics.uci.edu/pub/ietf/uri/#Related
-}
