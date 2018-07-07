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

import qualified Data.Geo.Jord as J
import qualified Data.List as L
import Prelude hiding (lookup)
import System.IO

newtype Vars =
    Vars [(String, J.Value)]

main :: IO ()
main = do
    hSetEncoding stdout utf8
    putStrLn
        ("jord interpreter, version " ++
         J.jordVersion ++ ": https://github.com/ofmooseandmen/jord  :? for help")
    loop (Vars [])
  where
    loop state = do
        input <- readI
        case input of
            ":quit" -> return ()
            ":q" -> return ()
            _ -> do
                let (result, newState) = evalS input state
                printS result
                loop newState

readI :: IO String
readI = putStr "\9783 " >> hFlush stdout >> getLine

printS :: Either String String -> IO ()
printS (Left err) = putStrLn ("\x1b[31m\9783 \x1b[30m" ++ err)
printS (Right "") = return ()
printS (Right r) = putStrLn ("\x1b[32m\9783 \x1b[30m" ++ r)

evalS :: String -> Vars -> (Either String String, Vars)
evalS s vs
    | null s = (Right "", vs)
    | (head s) == ':' = (evalC w vs)
    | (v:"=":e) <- w =
        let r = evalE (unwords e) vs
            vs' = save r v vs
         in (showR r, vs')
    | otherwise = (showR (evalE s vs), vs)
  where
    w = words s

evalC :: [String] -> Vars -> (Either String String, Vars)
evalC [":show", v] vs = (evalShow (Just v) vs, vs)
evalC [":show"] vs = (evalShow Nothing vs, vs)
evalC [":delete", v] vs = evalDel (Just v) vs
evalC [":clear"] vs = evalDel Nothing vs
evalC [":help"] vs = (Right help, vs)
evalC [":?"] vs = (Right help, vs)
evalC c vs = (Left ("Unsupported command " ++ unwords c ++ "; :? for help"), vs)

evalShow :: Maybe String -> Vars -> Either String String
evalShow (Just n) vs = maybe (Left ("Unbound variable: " ++ n)) (\e -> Right (show e)) (lookup n vs)
evalShow Nothing (Vars es) = Right ("vars:\n" ++ L.intercalate "\n" (map show es))

evalDel :: Maybe String -> Vars -> (Either String String, Vars)
evalDel _ vs = (Left "Unsupported", vs)

help :: String
help =
    "\njord interpreter, version " ++
    J.jordVersion ++
    "\n" ++
    "\n Commands available from the prompt:\n\n" ++
    "    :help, :?              display this list of commands\n" ++
    "    :quit, :q              quit jord\n" ++
    "    :show {var}            shows {var} or all variable(s) if no arg\n" ++
    "    :delete {var}          deletes {var}\n" ++
    "    :clear                 deletes all variable(s)\n" ++
    "\n Jord expressions:\n\n" ++
    "    (f x y) where f is one of function described below and x and y\n" ++
    "    are either parameters in one of the format described below or\n" ++
    "    a call to another function\n" ++
    "\n" ++
    "    (finalBearing (destination (antipode 54°N,154°E) 54° 1000m) (readGeoPos 54°N,154°E))\n" ++
    "\n" ++
    "    Top level () can be ommitted: antipode 54N028E\n" ++
    "\n -- Position calculations:\n\n" ++
    "    antipode pos               antipodal point of pos\n" ++
    "    distance pos1 pos2         surface distance between pos1 and pos2\n" ++
    "    destination pos len ang    destination position from pos having travelled len on initial bearing ang\n" ++
    "    initialBearing pos1 pos2   bearing arriving at pos2 from pos1\n" ++
    "    finalBearing pos1 pos2     initial bearing from pos1 to pos2\n" ++
    "    latlong pos                decimal latitude and longitude of pos\n" ++
    "    midpoint [pos]             mid position between [pos]\n" ++
    "    toNVector pos              n-vector corresponding to pos\n" ++
    "\n  Supported Position formats:\n" ++
    "        DD(MM)(SS)[N|S]DDD(MM)(SS)[E|W] - 553621N0130209E\n" ++
    "        d°m's\"[N|S],d°m's\"[E|W]         - 55°36'21\"N,13°2'9\"E\n" ++
    "          ^ zeroes can be ommitted and separtors can be any string\n" ++
    "        decimal°[N|S],decimal°[E|W]     - 51.885°N,13,1°E\n" ++
    "\n  Supported Angle formats:\n" ++
    "        d°m's    - 55°36'21.154\n" ++
    "        decimal° - 51.885°\n" ++
    "\n  Supported Length formats: {l}m, {l}km, {l}Nm\n" ++
    "\n  The result of every function can be saved by prefixing the function with \"{var} = \"\n" ++
    "  Saved results can subsequently be used when calling a function\n" ++
    "    jord> a = antipode 54N028E\n" ++ "    jord> antipode a\n"

evalE :: String -> Vars -> J.Result
evalE e vs = J.eval e (\s -> lookup s vs)

save :: J.Result -> String -> Vars -> Vars
save (Right a@(J.Ang _)) k vs = bind k a vs
save (Right l@(J.Len _)) k vs = bind k l vs
save (Right g@(J.Geo _)) k vs = bind k g vs
save (Right v@(J.Vec _)) k vs = bind k v vs
save (Right ll@(J.Ll _)) k vs = bind k ll vs
save _ _ vs = vs

showR :: J.Result -> Either String String
showR (Left err) = Left (show err)
showR (Right (J.Ang a)) = Right ("angle: " ++ (show a))
showR (Right (J.Len l)) = Right ("length: " ++ (show l))
showR (Right (J.Geo g)) = Right ("geo pos: " ++ (show g))
showR (Right (J.Vec v)) = Right ("n-vector: " ++ (show v))
showR (Right (J.Ll ll)) =
    Right ("latitude: " ++ (show (fst ll)) ++ "; longitude: " ++ (show (snd ll)))

-- variables
bind :: String -> J.Value -> Vars -> Vars
bind k v m = Vars (e ++ [(k, v)])
  where
    Vars e = unbind k m

lookup :: String -> Vars -> Maybe J.Value
lookup k (Vars es) = fmap snd (L.find (\e -> fst e == k) es)

unbind :: String -> Vars -> Vars
unbind k (Vars es) = Vars (filter (\e -> fst e /= k) es)
