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
import Data.List
import System.IO

data Result
    = Error String
    | OkPos GeoPos
    | OkLen Length
    | OkAng Angle
    | Help String

data Variables = Variables
    { positions :: [(String, GeoPos)]
    , lengths :: [(String, Length)]
    , angles :: [(String, Angle)]
    }

main :: IO ()
main = do
    printS
        ("jord, version " ++ jordVersion ++ ": https://github.com/ofmooseandmen/jord  :? for help")
    loop (Variables [] [] [])
  where
    loop state = do
        input <- readI
        case input of
            ":quit" -> return ()
            _ -> do
                let (result, newState) = evaluate input state
                printS result
                loop newState

readI :: IO String
readI = putStr "jord> " >> hFlush stdout >> getLine

printS :: String -> IO ()
printS = putStrLn

evaluate :: String -> Variables -> (String, Variables)
evaluate input vs =
    case w of
        (v:"=":_) ->
            let (r, vs') = save (run (drop 2 w) vs) v vs
             in (showR r, vs')
        _ -> (showR (run w vs), vs)
  where
    w = words input

run :: [String] -> Variables -> Result
run ["antipode", p] vs = evalAntipode p vs
run ["distance", p1, p2] vs = evalDistance p1 p2 vs
run ["initialBearing", p1, p2] vs = evalInitialBearing p1 p2 vs
run ["finalBearing", p1, p2] vs = evalFinalBearing p1 p2 vs
run ("midpoint":ps) vs = evalMidpoint ps vs
run [":help"] _ = help
run [":?"] _ = help
run cmd _ = Error ("Unsupported command: " ++ unwords cmd)

evalAntipode :: String -> Variables -> Result
evalAntipode s vs =
    case resolveAllPos [s] vs of
        [OkPos p] -> OkPos (antipode p)
        ps -> err ps

evalDistance :: String -> String -> Variables -> Result
evalDistance s1 s2 vs =
    case resolveAllPos [s1, s2] vs of
        [OkPos p1, OkPos p2] -> OkLen (distance p1 p2)
        ps -> err ps

evalInitialBearing :: String -> String -> Variables -> Result
evalInitialBearing s1 s2 vs =
    case resolveAllPos [s1, s2] vs of
        [OkPos p1, OkPos p2] -> OkAng (initialBearing p1 p2)
        ps -> err ps

evalFinalBearing :: String -> String -> Variables -> Result
evalFinalBearing s1 s2 vs =
    case resolveAllPos [s1, s2] vs of
        [OkPos p1, OkPos p2] -> OkAng (finalBearing p1 p2)
        ps -> err ps

evalMidpoint :: [String] -> Variables -> Result
evalMidpoint s vs =
    if hasErr ps
        then err ps
        else OkPos (midpoint [p | OkPos p <- ps])
  where
    ps = resolveAllPos s vs

help :: Result
help =
    Help
        ("\n Commands available from the prompt:\n\n" ++
         "    :help, :?    display this list of commands\n" ++
         "    :quit        quit jord\n" ++
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

resolveAllPos :: [String] -> Variables -> [Result]
resolveAllPos xs vs = map (`resolvePos` vs) xs

resolvePos :: String -> Variables -> Result
resolvePos s vs =
    case lookupPos s vs of
        Just (_, pos) -> OkPos pos
        Nothing -> maybe (Error ("Invalid position: " ++ s)) OkPos (readGeoPosM s)

save :: Result -> String -> Variables -> (Result, Variables)
save (OkPos g) v vs =
    if member v vs
        then (Error ("Variable already assigned: " ++ v), vs)
        else (OkPos g, insertPos v g vs)
save (OkLen l) v vs =
    if member v vs
        then (Error ("Variable already assigned: " ++ v), vs)
        else (OkLen l, insertLen v l vs)
save (OkAng a) v vs =
    if member v vs
        then (Error ("Variable already assigned: " ++ v), vs)
        else (OkAng a, insertAngle v a vs)
save r _ vs = (r, vs)

showR :: Result -> String
showR (Error s) = "jord> \x1b[31merror:\x1b[30m " ++ s
showR (OkPos g) = "jord> \x1b[32mposition:\x1b[30m " ++ show g
showR (OkLen l) = "jord> \x1b[32mlength:\x1b[30m " ++ show l
showR (OkAng a) = "jord> \x1b[32mangle:\x1b[30m " ++ show a
showR (Help h) = h

-- | Does given list of results contain 'Error'?
hasErr :: [Result] -> Bool
hasErr rs = not (null [e | Error e <- rs])

-- | Reduces all 'Error' results.
err :: [Result] -> Result
err rs = Error (intercalate "; " [e | Error e <- rs])

lookupPos :: String -> Variables -> Maybe (String, GeoPos)
lookupPos v vs =
    case filter (\a -> fst a == v) (positions vs) of
        [t] -> Just t
        _ -> Nothing

lookupLen :: String -> Variables -> Maybe (String, Length)
lookupLen v vs =
    case filter (\a -> fst a == v) (lengths vs) of
        [t] -> Just t
        _ -> Nothing

lookupAngle :: String -> Variables -> Maybe (String, Angle)
lookupAngle v vs =
    case filter (\a -> fst a == v) (angles vs) of
        [t] -> Just t
        _ -> Nothing

insertPos :: String -> GeoPos -> Variables -> Variables
insertPos v p vs = Variables (positions vs ++ [(v, p)]) (lengths vs) (angles vs)

insertLen :: String -> Length -> Variables -> Variables
insertLen v l vs = Variables (positions vs) (lengths vs ++ [(v, l)]) (angles vs)

insertAngle :: String -> Angle -> Variables -> Variables
insertAngle v a vs = Variables (positions vs) (lengths vs) (angles vs ++ [(v, a)])

member :: String -> Variables -> Bool
member v vs = length (filter (\a -> fst a == v) (positions vs)) == 1
