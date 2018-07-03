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

import Control.Monad (unless)
import Data.Geo.Jord
import Data.List
import System.IO

main :: IO ()
main = do
    input <- read'
    unless (input == ":quit") $ print' (eval' input) >> main

read' :: IO String
read' = putStr "jord> " >> hFlush stdout >> getLine

eval' :: String -> String
eval' input = input

print' :: String -> IO ()
print' = putStrLn

data Result = Error String | OkGeo GeoPos | OkLen Length

data Variables = Variables
    { positions :: [(String, GeoPos)]
    }

evaluate :: String -> Variables -> Result
evaluate cmd vs =
    case words cmd of
        ["antipode", p] -> evalAntipode p vs
        ["distance", p1, p2] -> evalDistance p1 p2 vs
        _ -> Error cmd

evalAntipode :: String -> Variables -> Result
evalAntipode s vs =
    case resolveAllPos [s] vs of
        [OkGeo p] -> OkGeo (antipode p)
        ps -> err ps

evalDistance :: String -> String -> Variables -> Result
evalDistance s1 s2 vs =
    case resolveAllPos [s1, s2] vs of
        [OkGeo p1, OkGeo p2] -> OkLen (distance p1 p2)
        ps -> err ps

resolveAllPos :: [String] -> Variables -> [Result]
resolveAllPos xs vs = map (\l -> resolvePos l vs) xs

resolvePos :: String -> Variables -> Result
resolvePos s vs =
    case filter (\a -> fst a == s) (positions vs) of
        [(_, pos)] -> (OkGeo pos)
        [] -> maybe (Error ("Invalid position: " ++ s)) OkGeo (readGeoPosM s) 
        _ -> Error (s ++ " defined more than once")

-- | Reduces all 'Error' results.
err :: [Result] -> Result
err rs = Error (intercalate "; " [e | Error e <- rs])