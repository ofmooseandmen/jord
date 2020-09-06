module Generator
    ( Header(..)
    , Generator(..)
    , generate
    , documentation
    ) where

data Header =
    Header
        { comment :: [String]
        , module' :: String
        }

data Generator a =
    Generator [String] (a -> String) ([a] -> String)

generate :: Header -> Generator a -> [a] -> String
generate h (Generator imports genElt genAll) elts =
    header h ++
    "module " ++
    module' h ++
    " where\n\n" ++
    unlines (map ("import " ++) imports) ++ "\n" ++ unlines (map (\e -> genElt e ++ "\n") elts) ++ genAll elts

header :: Header -> String
header h =
    "-- | \n\
    \-- Module:      " ++
    module' h ++
    " \n" ++
    "-- Copyright:   (c) 2020 Cedric Liegeois \n\
    \-- License:     BSD3 \n\
    \-- Maintainer:  Cedric Liegeois <ofmooseandmen@yahoo.fr> \n\
    \-- Stability:   experimental \n\
    \-- Portability: portable \n\
    \--\n" ++
    genComment (comment h) ++
    "--\n\
    \-- This module has been generated.\n"

documentation :: [String] -> String
documentation [] = ""
documentation (c:cs) = ("-- |" ++ c ++ "\n") ++ genComment cs

genComment :: [String] -> String
genComment cs = unlines (map ("--" ++) cs)