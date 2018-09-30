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
import Data.List ((\\), dropWhileEnd, isPrefixOf)
import Eval
import Prelude hiding (lookup)
import Show
import State
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
    runInputT mySettings $ withInterrupt $ loop emptyState
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

evalS :: String -> State -> (Either String String, State)
evalS s state
    | null s = (Right "", state)
    | head s == ':' = evalC w state
    | (v:"=":e) <- w =
        if v `elem` functions
            then (Left (v ++ " is a reserved keyword"), state)
            else let r = eval (unwords e) state
                     state' = save r v state
                  in (showR r state', state')
    | otherwise = (showR (eval s state) state, state)
  where
    w = words s

evalC :: [String] -> State -> (Either String String, State)
evalC [":help"] state = (Right (help state), state)
evalC [":?"] state = (Right (help state), state)
evalC [":show", v] state = (evalShow v state, state)
evalC [":delete", v] state = (Right ("deleted var: " ++ v), delete v state)
evalC [":units", u1, u2] state = evalUnits [u1, u2] state
evalC [":units", u] state = evalUnits [u] state
evalC [":units"] state = showUnits state
evalC [":reset"] _ = (Right "REPL reset ", emptyState)
evalC c state = (Left ("Unsupported command " ++ unwords c ++ "; :? for help"), state)

evalShow :: String -> State -> Either String String
evalShow n state =
    maybe (Left ("Unbound variable: " ++ n)) (\v -> Right (showVar n v state)) (lookup n state)

evalUnits :: [String] -> State -> (Either String String, State)
evalUnits us s = showUnits (setUnits us s)

showUnits :: State -> (Either String String, State)
showUnits s = (Right ("Units:\n  length = " ++ lengthUnit s ++ "\n  speed  = " ++ speedUnit s), s)

help :: State -> String
help s =
    "\njord interpreter, version " ++
    jordVersion ++
    "\n" ++
    "\n Commands available from the prompt:\n\n" ++
    "    :help, :?              display this list of commands\n" ++
    "    :quit, :q              quit jord\n" ++
    "    :show {var}            show {var}\n" ++
    "    :delete {var}          delete {var}\n" ++
    "    :units length speed    set length and speed units used for display\n" ++
    "                           see supported length and speed format\n" ++
    "                           currently: length = " ++
    lengthUnit s ++
    "; speed = " ++
    speedUnit s ++
    "\n" ++
    "    :units                 show length and speed units used for display\n" ++
    "    :reset                 reset REPL to default state (including deleting all variables)\n" ++
    "\n Jord expressions:\n\n" ++
    "    (f x y) where f is one of function described below and x and y\n" ++
    "    are either parameters in one of the format described below or\n" ++
    "    a call to another function\n" ++
    "\n" ++
    "    (finalBearing (destination (antipode 54°N,154°E) 54° 1000m) 54°N,154°E)\n" ++
    "\n" ++
    "    Top level () can be ommitted: antipode 54N028E\n" ++
    "\n  Position calculations (Frames):\n\n" ++
    "     The following calculations work with both ellipsoidal and derived earth model\n" ++
    "     WGS84 ellipsoid is used if model is omitted\n" ++
    "\n     deltaBetween pos1 pos2 frame (earth)  delta between pos1 and pos2 in frame originating at pos1\n" ++
    "     nedBetween pos1 pos2 (earth)          NED between pos1 and pos2 in frame N originating at pos1\n" ++
    "     target pos frame delta (earth)        target position from pos and delta in frame originating at pos\n" ++
    "     targetN pos delta (earth)             target position from pos and NED in frame N originating at pos\n" ++
    "\n  Position calculations (Spherical Earth):\n\n" ++
    "     The following calculations assume a spherical earth model with a radius\n" ++
    "     derived from the WGS84 ellipsoid: " ++
    showLength r84 s ++
    "\n" ++
    "\n     alongTrackDistance pos ga           signed distance of pos to perpendicular of great arc ga\n" ++
    "     antipode pos                          antipodal point of pos\n" ++
    "     crossTrackDistance pos gc             signed distance from pos to great circle gc\n" ++
    "     destination pos ang len               destination position from pos having travelled len\n" ++
    "                                           on initial bearing ang (either in text form or decimal degrees)\n" ++
    "     finalBearing pos1 pos2                initial bearing from pos1 to pos2\n" ++
    "     initialBearing pos1 pos2              bearing arriving at pos2 from pos1\n" ++
    "     interpolate pos1 pos2 (0..1)          position at fraction between pos1 and pos2\n" ++
    "     intersections gc1 gc2                 intersections between great circles gc1 and gc2\n" ++
    "                                           exactly 0 or 2 intersections\n" ++
    "     insideSurface pos [pos]               is p inside surface polygon?\n" ++
    "     mean [pos]                            geographical mean surface position of [pos]\n" ++
    "     surfaceDistance pos1 pos2             surface distance between pos1 and pos2\n" ++
    "\n  Kinematics calculations (Spherical Earth):\n\n" ++
    "     The following calculations assume a spherical earth model with a radius\n" ++
    "     derived from the WGS84 ellipsoid: " ++
    showLength r84 s ++
    "\n" ++
    "\n     position track dur                    position of track after duration\n" ++
    "     cpa track1 track2                     closest point of approach between two tracks\n" ++
    "     intercept track pos                   minimum speed of interceptor at pos to intercept target\n" ++
    "     intercept track pos spd               time needed by interceptor at pos and travelling at spd to intercept target\n" ++
    "     intercept track pos dur               speed needed by interceptor at pos to intercept target after duration\n" ++
    "\n  Constructors and conversions:\n\n" ++
    "     ecef len len len                      earth-centred earth-fixed position from x, y, z lengths\n" ++
    "     ecef metres metres metres             earth-centred earth-fixed position from x, y, z metres\n" ++
    "     toEcef pos (earth)                    geographic position to ECEF position using earth model\n" ++
    "                                           WGS84 ellipsoid is used if model is omitted\n" ++
    "     fromEcef ecef (earth)                 ECEF position to geographic position using earth model\n" ++
    "                                           WGS84 ellipsoid is used if model is omitted\n" ++
    "     frameB ang ang ang                    body frame (vehicle) from yaw, pitch and roll angles\n" ++
    "     frameL ang                            local frame from wander azimuth angle\n" ++
    "     frameN                                north, east, down frame\n" ++
    "     delta len len len                     delta from lengths\n" ++
    "     delta metres metres metres            delta from metres\n" ++
    "     ned len len len                       north, east, down from lengths\n" ++
    "     ned metres metres metres              north, east, down from metres\n" ++
    "     geo latlong                           surface geographic position from latlong\n" ++
    "     geo latlong height                    geographic position from latlong and height\n" ++
    "     geo lat long height                   geographic position from decimal latitude, longitude and height\n" ++
    "     geo lat long metres                   geographic position from decimal latitude, longitude and metres\n" ++
    "     toNVector pos                         n-vector corresponding to pos\n" ++
    "     greatArc pos1 pos2                    great arc passing by pos1 and pos2\n" ++
    "     greatArc track dur                    great arc from track and duration\n" ++
    "     greatCircle pos1 pos2                 great circle passing by pos1 and pos2\n" ++
    "     greatCircle pos1 pos2                 great circle passing by pos1 and pos2\n" ++
    "     greatCircle pos ang                   great circle passing by pos and heading on bearing ang\n" ++
    "     greatCircle ga                        great circle from great arc\n" ++
    "     greatCircle track                     great circle from track\n" ++
    "     track pos ang spd                     track at pos, heading on bearing ang and travelling at speed spd\n" ++
    "\n  Supported lat/long formats:\n\n" ++
    "    DD(MM)(SS)[N|S]DDD(MM)(SS)[E|W] - 553621N0130209E\n" ++
    "    d°m's\"[N|S],d°m's\"[E|W]         - 55°36'21\"N,13°2'9\"E\n" ++
    "    ^ zeroes can be ommitted and separtors can also be d, m, s\n" ++
    "    decimal°[N|S],decimal°[E|W]     - 51.885°N,13,1°E\n" ++
    "\n  Supported angle formats:\n\n" ++
    "    d°m's    - 55°36'21.154\n" ++
    "    decimal° - 51.885°\n" ++
    "\n  Supported length formats: {l}m, {l}km, {l}nm, {l}ft\n" ++
    "\n  Supported speed formats: {s}m/s, {s}km/h, {s}mph, {s}kt, {s}ft/s\n" ++
    "\n  Supported duration formats: (-)nHnMn.nS\n" ++
    "\n  Supported earth models:\n\n" ++
    "    ellipsoidal: wgs84, grs80, wgs72\n" ++
    "    spherical  : s84, s80, s72\n" ++
    "\n\n  Every evaluated result can be saved by prefixing the expression with \"{var} = \"\n" ++
    "  Saved results can subsequently be used when calling a function\n" ++
    "\n  Examples:\n\n" ++
    "    jord> a = antipode 54N028E\n" ++
    "    jord> antipode a\n" ++
    "    jord> f = frameB 10d 20d 30d\n" ++
    "    jord> d = delta 3000 2000 100\n" ++
    "    jord> p0 = geo 49.66618 3.45063 0\n" ++ "    jord> target p0 f d wgs84\n"

save :: Result -> String -> State -> State
save (Right v) k state = insert k v state
save _ _ state = state

showR :: Result -> State -> Either String String
showR (Left err) _ = Left err
showR (Right v) s = Right (showV v s)

showVar :: String -> Value -> State -> String
showVar n v s = n ++ "=" ++ showV v s
