-- |
-- Module:      Main
-- Copyright:   (c) 2018 Cedric Liegeois
-- License:     BSD3
-- Maintainer:  Cedric Liegeois <ofmooseandmen@yahoo.fr>
-- Stability:   experimental
-- Portability: portable
--
-- REPL around "Jord".
--
module Main where

import Data.Geo.Jord
import Data.List ((\\), dropWhileEnd, intercalate, isPrefixOf)
import Eval
import Prelude hiding (lookup)
import System.Console.Haskeline

search :: String -> [Completion]
search s = map simpleCompletion $ filterFunc s

filterFunc :: String -> [String]
filterFunc s = map (\f -> pref ++ f) filtered
  where
    pref = dropWhileEnd (/= '(') s -- everything before the last '(' inclusive
    func = (\\) s pref -- everything after the last '('
    filtered = filter (\f -> func `isPrefixOf` f) functions

mySettings :: Settings IO
mySettings =
    Settings
        { complete = completeWord Nothing " \t" $ return . search
        , historyFile = Nothing
        , autoAddHistory = True
        }

main :: IO ()
main = do
    putStrLn
        ("jord interpreter, version " ++
         jordVersion ++ ": https://github.com/ofmooseandmen/jord  :? for help")
    runInputT mySettings $ withInterrupt $ loop emptyVault
  where
    loop state = do
        input <- handleInterrupt (return (Just "")) $ getInputLine "jord> "
        case input of
            Nothing -> return ()
            Just ":quit" -> return ()
            Just ":q" -> return ()
            Just i -> do
                let (result, newState) = evalS i state
                printS result
                loop newState

printS :: Either String String -> InputT IO ()
printS (Left err) = outputStrLn ("jord> " ++ err)
printS (Right "") = return ()
printS (Right r) = outputStrLn ("jord> " ++ r)

evalS :: String -> Vault -> (Either String String, Vault)
evalS s vault
    | null s = (Right "", vault)
    | head s == ':' = evalC w vault
    | (v:"=":e) <- w =
        let r = eval (unwords e) vault
            vault' = save r v vault
         in (showR r, vault')
    | otherwise = (showR (eval s vault), vault)
  where
    w = words s

evalC :: [String] -> Vault -> (Either String String, Vault)
evalC [":show", v] vault = (evalShow v vault, vault)
evalC [":delete", v] vault = evalDel (Just v) vault
evalC [":clear"] vault = evalDel Nothing vault
evalC [":help"] vault = (Right help, vault)
evalC [":?"] vault = (Right help, vault)
evalC c vault = (Left ("Unsupported command " ++ unwords c ++ "; :? for help"), vault)

evalShow :: String -> Vault -> Either String String
evalShow n vault = maybe (Left ("Unbound variable: " ++ n)) (Right . showVar n) (lookup n vault)

evalDel :: Maybe String -> Vault -> (Either String String, Vault)
evalDel (Just n) vault = (Right ("deleted var: " ++ n), delete n vault)
evalDel Nothing _ = (Right "deleted all variable ", emptyVault)

help :: String
help =
    "\njord interpreter, version " ++
    jordVersion ++
    "\n" ++
    "\n Commands available from the prompt:\n\n" ++
    "    :help, :?              display this list of commands\n" ++
    "    :quit, :q              quit jord\n" ++
    "    :show {var}            shows {var}\n" ++
    "    :delete {var}          deletes {var}\n" ++
    "    :clear                 deletes all variable(s)\n" ++
    "\n Jord expressions:\n\n" ++
    "    (f x y) where f is one of function described below and x and y\n" ++
    "    are either parameters in one of the format described below or\n" ++
    "    a call to another function\n" ++
    "\n" ++
    "    (finalBearing (destination (antipode 54°N,154°E) 54° 1000m) 54°N,154°E)\n" ++
    "\n" ++
    "    Top level () can be ommitted: antipode 54N028E\n" ++
    "\n  Position calculations (Spherical Earth):\n\n" ++
    "     The following calculations assume a spherical earth model with a radius\n" ++
    "     derived from the WGS84 ellipsoid: " ++
    show r84 ++
    "\n" ++
    "\n       antipode pos                   antipodal point of pos\n" ++
    "       crossTrackDistance pos gc      signed distance from pos to great circle gc\n" ++
    "       destination pos ang len        destination position from pos having travelled len\n" ++
    "                                      on initial bearing ang (either in text form or decimal degrees)\n" ++
    "       finalBearing pos1 pos2         initial bearing from pos1 to pos2\n" ++
    "       initialBearing pos1 pos2       bearing arriving at pos2 from pos1\n" ++
    "       interpolate pos1 pos2 (0..1)   position at fraction between pos1 and pos2\n" ++
    "       intersections gc1 gc2          intersections between great circles gc1 and gc2\n" ++
    "                                      exactly 0 or 2 intersections\n" ++
    "       insideSurface pos [pos]        is p inside surface polygon?\n" ++
    "       mean [pos]                     geographical mean surface position of [pos]\n" ++
    "       surfaceDistance pos1 pos2      surface distance between pos1 and pos2\n" ++
    "\n  Constructors and conversions:\n\n" ++
    "       geoPos latlong                 surface geographic position from latlong\n" ++
    "       geoPos latlong height          geographic position from latlong and height\n" ++
    "       geoPos lat long height         geographic position from decimal latitude, longitude and height\n" ++
    "       geoPos lat long metres         geographic position from decimal latitude, longitude and metres\n" ++
    "       greatCircle pos1 pos2          great circle passing by pos1 and pos2\n" ++
    "       greatCircle pos ang            great circle passing by pos and heading on bearing ang\n" ++
    "       toKilometres len               length to kilometres\n" ++
    "       toMetres len                   length to metres\n" ++
    "       toNauticalMiles len            length to nautical miles\n" ++
    "       toNVector pos                  n-vector corresponding to pos\n" ++
    "\n  Supported Lat/Long formats:\n\n" ++
    "       DD(MM)(SS)[N|S]DDD(MM)(SS)[E|W] - 553621N0130209E\n" ++
    "       d°m's\"[N|S],d°m's\"[E|W]         - 55°36'21\"N,13°2'9\"E\n" ++
    "         ^ zeroes can be ommitted and separtors can also be d, m, s\n" ++
    "       decimal°[N|S],decimal°[E|W]     - 51.885°N,13,1°E\n" ++
    "\n  Supported Angle formats:\n\n" ++
    "       d°m's    - 55°36'21.154\n" ++
    "       decimal° - 51.885°\n" ++
    "\n  Supported Length formats: {l}m, {l}km, {l}Nm\n\n" ++
    "\n  Every evaluated result can be saved by prefixing the expression with \"{var} = \"\n" ++
    "  Saved results can subsequently be used when calling a function\n" ++
    "    jord> a = antipode 54N028E\n" ++ "    jord> antipode a\n"

save :: Result -> String -> Vault -> Vault
save (Right v) k vault = insert k v vault
save _ _ vault = vault

showR :: Result -> Either String String
showR (Left err) = Left err
showR (Right v) = Right (showV v)

showV :: Value -> String
showV (Ang a) = "angle: " ++ show a ++ " (" ++ show (toDecimalDegrees a) ++ ")"
showV (Bool b) = show b
showV (Double d) = show d
showV (Gc gc) = "great circle: " ++ show gc
showV (Len l) = "length: " ++ show l
showV (Geo g) =
    "latitude, longitude: " ++
    show ll ++
    " (" ++
    show (toDecimalDegrees (latitude ll)) ++
    ", " ++ show (toDecimalDegrees (longitude ll)) ++ ") - height: " ++ show h
  where
    ll = pos g
    h = height g
showV (NVec nv) =
    "n-vector: (" ++ show x ++ ", " ++ show y ++ ", " ++ show z ++ ") - height: " ++ show h
  where
    v = vec (pos nv)
    x = vx v
    y = vy v
    z = vz v
    h = height nv
showV (Vals []) = "empty"
showV (Vals vs) = "\n  " ++ intercalate "\n  " (map showV vs)

showVar :: String -> Value -> String
showVar n v = n ++ "=" ++ showV v
