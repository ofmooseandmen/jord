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
import qualified Data.List as L
import Prelude hiding (lookup)
import System.IO

data Result
    = Error String
    | Help String
    | OkAng Angle
    | OkLen Length
    | OkPos GeoPos
    | ShowVars [(String, Var)]

data Var
    = VarAng Angle
    | VarLen Length
    | VarPos GeoPos
    deriving (Show)

newtype Vars =
    Vars [(String, Var)]

main :: IO ()
main = do
    hSetEncoding stdout utf8
    printS
        ("jord, version " ++ jordVersion ++ ": https://github.com/ofmooseandmen/jord  :? for help")
    loop (Vars [])
  where
    loop state = do
        input <- readI
        case input of
            ":quit" -> return ()
            ":q" -> return ()
            _ -> do
                let (result, newState) = evaluate input state
                printS result
                loop newState

readI :: IO String
readI = putStr "jord> " >> hFlush stdout >> getLine

printS :: String -> IO ()
printS = putStrLn

evaluate :: String -> Vars -> (String, Vars)
evaluate input vs =
    case w of
        (v:"=":_) ->
            let (r, vs') = save (run (drop 2 w) vs) v vs
             in (showR r, vs')
        _ -> (showR (run w vs), vs)
  where
    w = words input

run :: [String] -> Vars -> Result
run ["antipode", p] vs = evalAntipode p vs
run ["distance", p1, p2] vs = evalDistance p1 p2 vs
run ["initialBearing", p1, p2] vs = evalInitialBearing p1 p2 vs
run ["finalBearing", p1, p2] vs = evalFinalBearing p1 p2 vs
run ("midpoint":ps) vs = evalMidpoint ps vs
run [":show", v] vs = evalShow (Just v) vs
run [":show"] vs = evalShow Nothing vs
run [":delete", v] vs = evalDel (Just v) vs
run [":clear"] vs = evalDel Nothing vs
run [":help"] _ = help
run [":?"] _ = help
run cmd _ = Error ("Unsupported command: " ++ unwords cmd)

evalAntipode :: String -> Vars -> Result
evalAntipode s vs =
    case resolveAllPos [s] vs of
        [OkPos p] -> OkPos (antipode p)
        ps -> err ps

evalDistance :: String -> String -> Vars -> Result
evalDistance s1 s2 vs =
    case resolveAllPos [s1, s2] vs of
        [OkPos p1, OkPos p2] -> OkLen (distance p1 p2)
        ps -> err ps

evalInitialBearing :: String -> String -> Vars -> Result
evalInitialBearing s1 s2 vs =
    case resolveAllPos [s1, s2] vs of
        [OkPos p1, OkPos p2] -> OkAng (initialBearing p1 p2)
        ps -> err ps

evalFinalBearing :: String -> String -> Vars -> Result
evalFinalBearing s1 s2 vs =
    case resolveAllPos [s1, s2] vs of
        [OkPos p1, OkPos p2] -> OkAng (finalBearing p1 p2)
        ps -> err ps

evalMidpoint :: [String] -> Vars -> Result
evalMidpoint s vs =
    if hasErr ps
        then err ps
        else OkPos (midpoint [p | OkPos p <- ps])
  where
    ps = resolveAllPos s vs

evalShow :: Maybe String -> Vars -> Result
evalShow (Just n) vs =
    maybe (Error ("Unbound variable: " ++ n)) (\v -> ShowVars [(n, v)]) (lookup n vs)
evalShow Nothing (Vars e) = ShowVars e

evalDel :: Maybe String -> Vars -> Result
evalDel _ _ = Error "Unsupported"

help :: Result
help =
    Help
        ("\n Commands available from the prompt:\n\n" ++
         "    :help, :?              display this list of commands\n" ++
         "    :quit, :q              quit jord\n" ++
         "    :show {var}            shows {var} or all variable(s) if no arg\n" ++
         "    :delete {var}          deletes {var}\n" ++
         "    :clear                 deletes all variable(s)\n" ++
         "\n -- Position calculations:\n\n" ++
         "    antipode :: Position -> Position\n" ++
         "    distance :: Position -> Position -> Length\n" ++
         "    initialBearing :: Position -> Position -> Angle\n" ++
         "    finalBearing :: Position -> Position -> Angle\n" ++
         "    midpoint :: [Position] -> Position\n" ++
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
         "    jord> a = antipode 54N028E\n" ++ "    jord> antipode a\n")

resolveAllPos :: [String] -> Vars -> [Result]
resolveAllPos xs vs = map (`resolvePos` vs) xs

resolvePos :: String -> Vars -> Result
resolvePos s vs =
    case lookup s vs of
        Just (VarPos pos) -> OkPos pos
        Just _ -> Error ("Variable [" ++ s ++ "] is not a position")
        Nothing -> maybe (Error ("Invalid position: " ++ s)) OkPos (readGeoPosM s)

save :: Result -> String -> Vars -> (Result, Vars)
save (OkAng a) v vs = (OkAng a, bind v (VarAng a) vs)
save (OkLen l) v vs = (OkLen l, bind v (VarLen l) vs)
save (OkPos g) v vs = (OkPos g, bind v (VarPos g) vs)
save r _ vs = (r, vs)

showR :: Result -> String
showR (Error s) = "jord> \x1b[31merror:\x1b[30m " ++ s
showR (OkPos g) = "jord> \x1b[32mposition:\x1b[30m " ++ show g
showR (OkLen l) = "jord> \x1b[32mlength:\x1b[30m " ++ show l
showR (OkAng a) = "jord> \x1b[32mangle:\x1b[30m " ++ show a
showR (ShowVars [e]) = "jord> \x1b[32mvar:\x1b[30m " ++ fst e ++ " = " ++ show (snd e)
showR (ShowVars vs) =
    L.intercalate
        "\n"
        (map (\e -> "jord> \x1b[32mvar:\x1b[30m " ++ fst e ++ " = " ++ show (snd e)) vs)
showR (Help h) = h

-- | Does given list of results contain 'Error'?
hasErr :: [Result] -> Bool
hasErr rs = not (null [e | Error e <- rs])

-- | Reduces all 'Error' results.
err :: [Result] -> Result
err rs = Error (L.intercalate "; " [e | Error e <- rs])

-- variables
bind :: String -> Var -> Vars -> Vars
bind k v m = Vars (e ++ [(k, v)])
  where
    Vars e = unbind k m

lookup :: String -> Vars -> Maybe Var
lookup k (Vars es) = fmap snd (L.find (\e -> fst e == k) es)

unbind :: String -> Vars -> Vars
unbind k (Vars es) = Vars (filter (\e -> fst e /= k) es)
