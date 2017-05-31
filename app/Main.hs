{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE GADTs, OverloadedStrings, TypeFamilies, TemplateHaskell, QuasiQuotes, FlexibleInstances, StandaloneDeriving #-}
module Main where

import qualified AXConfigDB as AXC (readConfig, Config(..))
import Data.Char
import Data.List()
import Prelude.Unicode
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Control.Monad
import Control.Exception (catch)
import Control.Monad.Trans.Either(EitherT(..), runEitherT, hoistEither, left)
import qualified Data.ByteString.Char8 as BSC8 (ByteString, putStrLn, pack, unpack)
import Data.IORef
import Data.Time.Clock.POSIX
-- import Network.URI
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import qualified Database.PostgreSQL.Simple as PGS(Connection, connect,execute,execute_,query,query_, SqlError(..))
import Control.Monad.IO.Class (liftIO)
--import Database.Categories
import Database.URLSchemes as URLSch
import qualified Database as DB
import System.Directory
import System.Environment
import System.Exit
import System.IO
import System.Console.GetOpt
import URL
-- import AXOptions
import Opaleye as OP(Column, Nullable, matchNullable, isNull,
                          Table(Table), required, queryTable,
                          Query, QueryArr, restrict, (.==), (.<=), (.&&), (.<),
                          (.===), (.++), ifThenElse, pgString, aggregate, groupBy,
                          count, avg, sum, leftJoin, runQuery,
                          showSqlForPostgres, Unpackspec,PGUuid, PGInt4, PGInt8, PGText, PGDate, PGFloat8, PGBool)
import Options as Opt(Options(..), cases, defaultOptions, options)
{-
display st = do
  (dy, _, _) <- Graphics.UI.GLUT.get st
  clear [ColorBuffer]
---  currentColor $= Color4 1 0 0 1
  renderPrimitive Triangles $ do
    vertex $ Vertex3 (0    :: GLdouble) ( 0.5 + dy) 0
    vertex $ Vertex3 (0.5  :: GLdouble) (-0.5 + dy) 0
    vertex $ Vertex3 (-0.5 :: GLdouble) (-0.5 + dy) 0
--    swapBuffers
-- flush

      corner 1 0 0 5 5
    corner 0 1 0 25 5
    corner 0 0 1 5 25


idle st = do
  (dy, delta, prevTStamp) <- Graphics.UI.GLUT.get st
  tstamp <- getTimestamp
  let dt = tstamp - prevTStamp
      dy' = dy + delta * dt
      delta' = if abs dy' <= 0.5 then delta else -delta
  st $=! (dy', delta', tstamp)
  postRedisplay Nothing
-}

handler ∷ PGS.SqlError → IO [a]
handler e = do
  BSC8.putStrLn . PGS.sqlErrorMsg $ e
  return []

getTimestamp :: IO GLdouble
getTimestamp = do
  now <- getPOSIXTime
  return $ fromRational $ toRational now

increase :: Int → Int
increase year = year + 1

f [] = []
f y@(x:xs) = if x == 'a' || x == 'c' then 'b' : (f xs) else y

dispatch :: [(String, String → IO ())]  
dispatch =  [   ("add"     , add   ),
                ("delete"  , remove),
                ("--help"    , viewHelp),
                ("view"    , view  )
            ]

requestMode, getUserInterface∷ String → Options →  IO Options
requestMode arg opt = return opt { optRequest = arg }
readInput arg opt = return opt { optInput = readFile arg }
writeOutput arg opt = return opt { optOutput = writeFile arg }
getUserInterface arg opt = return opt { optUserInterface = arg }

optionArgs = [
    ((ReqArg requestMode "default"), "Режим запуска"),
    ((NoArg showHelp), "Справка"),
    ((ReqArg readInput "FILE"  ), "some option that requires a file argument"),
    ((ReqArg writeOutput "FILE"), "some option with an optional file argument"),
    ((ReqArg getUserInterface "FILE" ), "Интерфейс программы"),
    ((NoArg showVersion        ), "show version number")
  ]
add :: String → IO ()
add link = putStrLn $ "Добавление " ++ link

remove :: String → IO ()
remove s = putStrLn "Удаление"

view ::String → IO ()
view s = putStrLn "Просмотр "

viewHelp :: String → IO ()
viewHelp  s = putStrLn "Справка"

existsWhere::Connection → String → (String,String) → (IO Bool)
existsWhere c t (f, v)=do
  rs ← query c "select count (*) from url_schemes where abbreviation = ? limit 1" (Only v) :: IO [Only Int] -- ("select count(*) from " ++ t ++ " where " ++ f ++ " = '" ++ v ++ "' limit 1"))  :: IO [Only Int]
  putStrLn . show . fromOnly $ rs !! 0
-- return True
  if ((fromOnly $ rs !! 0)::Int) > 0 then return True else return False
--    do
--      rs2 ← execute c "insert into url_schemes (id,abbreviation) values (nextval('url_schemes_ids'),?)" (Only v)  -- nextval('url_schemes_ids')
--      putStrLn $ show rs2
      

main = do
        args ← getArgs
--        let (Just sa) = sequence $ map ((flip lookup) dispatch) args

--        let (Just action) = maplookup command dispatch
--        action $ args !! 0
        cfg ← AXC.readConfig
        let dbc = AXC.db cfg
        conn ← PGS.connect dbc
        let ( actions, nonOpts, msgs ) = getOpt RequireOrder (zipWith (\f (x,y) → f x y) options optionArgs) args
        opts ← foldl (>>=) (return defaultOptions) actions
        let Options {
                    optRequest = req,
                    optInput = input,
                    optOutput = output } = opts
        getHandler conn req
        input >>= output
{-
        case cfg of
             (Right cs) → do
                  let dbs = lookup "Database" cs
                  case dbs of
                      (Just se) → do
                            lineFromUser ← getLine
                            let lsd = parseURL lineFromUser
                            putStrLn .show $ lsd
                            case lsd of
                                (Just x) → do
                                        let mbCI = getConInf se
                                        case mbCI of
                                            (Just ci) → do
                                              conn ← connect ci
                                              rs3 ← existsWhere conn "url_schemes" ("abbreviation", scheme x)
                                              putStrLn $ show rs3
                                            Nothing → error "Nothing"
                                Nothing → error "Nothing"
                      Nothing → error "Секция Database не найдена в /data-soml/config.soml"
             (Left ss) → putStrLn $ show ss
             -}

             --            withMySQLConn getConInf $ runDbConn $ do
-- runMigration (migrate abbt)
{-
--minURL <- read lsd::URL
--                putStrLn . show $ minURL
                johnOrders <- select (AbbreviationField ==. "http")
                
                if length johnOrders > 0 then
                    liftIO $ putStrLn $ "table: " ++ show (URLSch.id ( johnOrders !! 0))
                else
                    insert $ UrlSchemes {URLSch.id = 5, abbreviation = "http"}-}



{-  _ <- getArgsAndInitialize
  _ <- createWindow "Jumping Triangle"
  tstamp <- getTimestamp
  st <- newIORef (0.0, 0.5, tstamp)
  displayCallback $= display st
  idleCallback $= Just (idle st)
  mainLoop
-}

{-        getArgsAndInitialize
        createWindow "Triangle"
        displayCallback $= display
        matrixMode $= Projection
        loadIdentity
        ortho2D 0 30 0 30
        matrixMode $= Modelview 0
        mainLoop

 print $ fmap' (toLower) $ WorldConstr 'E'
 putStrLn . show $ WorldConstr  (toLower 'F')
-- putStrLn . show $ fmap' (fmap' toLower) $ [MiniWorld 'M', WorldConstr 'S']

-}
corner r g b x y = do color  (Color3  r g b :: Color3  GLfloat)
                      vertex (Vertex2 x y   :: Vertex2 GLfloat)

showHelp, showVersion ∷ Options → IO Options
showHelp _ = do
  putStrLn "Справка"
  exitWith ExitSuccess

showVersion _ = do
  putStrLn "v0.1"
  exitWith ExitSuccess

getHandler ∷ PGS.Connection → String → IO()
getHandler conn "GetLink" = do
    putStrLn "Для"
    putStrLn "   - получения справки нажмите F1"
    putStrLn "   - добавления пароля ссылки нажмите F2 + c"
    putStrLn "   - получения ссылки введите название метки"
--    HL.runInputT HL.defaultSettings repl
    
    ss <- getLine
    ei <- runEitherT $ getDomain conn ss
    case ei of
                 Right x -> do
                     print x
                 Left m -> putStrLn m
    return ()
getHandler conn _ = do
    putStrLn "Режим не выбран"

getDomain ∷ PGS.Connection → String → EitherT String IO DB.Domain
getDomain conn ss = do
            d2 <- liftIO (catch ( do
                    d <- (runQuery conn (DB.getDomainRows ss)) ∷ IO [DB.Domain]
                    print d
                    return d)
                handler)
            let md = listToCode d2
            case md of
                 Right x -> return x
                 Left m -> left m
    where
        listToCode ∷ [DB.Domain] → Either String DB.Domain
        listToCode [] = Left "Запись не найдена"
        listToCode [x] = Right x
        listToCode xs = Left "Найдено слишком много (более одного)"
