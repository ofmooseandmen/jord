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
import System.IO

main :: IO ()
main = do
    input <- read'
    unless (input == ":quit") $ print' (eval' input) >> main

read' :: IO String
read' = putStr "jord> " >> hFlush stdout >> getLine

eval' :: String -> String
eval' input =
    case words input of
        ["antipode", p] -> antipode' p
        ["distance", p1, p2] -> distance' p1 p2
        _ -> help

print' :: String -> IO ()
print' = putStrLn

help :: String
help = "Usage: antipode Position" ++ "\n       distance Postion Position" ++ "\n       :quit"

antipode' :: String -> String
antipode' s = maybe ("Invalid position: " ++ s) (antipode >> show) (readGeoM s)

distance' :: String -> String -> String
distance' s1 s2 =
    case ps of
        Nothing -> "Invalid position(s): " ++ s1 ++ ", " ++ s2
        Just [p1, p2] -> show (distance p1 p2)
        Just _ -> "Something went wrong..."
  where
    ps = mapM readGeoM [s1, s2]
