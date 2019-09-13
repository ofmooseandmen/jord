module Generator
    ( Header(..)
    , Generator(..)
    , generate
    , commentToString
    ) where

data Header =
    Header
        { comment :: [String]
        , module' :: String
        }

data Generator a =
    Generator
        { imports :: [String]
        , toString :: a -> String
        }

generate :: Header -> Generator a -> [a] -> String
generate h (Generator is ts) elts =
    header h ++
    "module " ++
    module' h ++
    " where\n\n" ++
    unlines (map (\i -> "import " ++ i) is) ++ "\n" ++ unlines (map (\e -> ts e ++ "\n") elts)

header :: Header -> String
header h =
    "-- | \n\
    \-- Module:      " ++
    module' h ++
    " \n" ++
    "-- Copyright:   (c) 2019 Cedric Liegeois \n\
    \-- License:     BSD3 \n\
    \-- Maintainer:  Cedric Liegeois <ofmooseandmen@yahoo.fr> \n\
    \-- Stability:   experimental \n\
    \-- Portability: portable \n\
    \--\n" ++
    commentToString (comment h) ++
    "--\n\
    \-- This module has been generated.\n\
    \--\n"

commentToString :: [String] -> String
commentToString c = unlines (map (\s -> "--" ++ s) c)
