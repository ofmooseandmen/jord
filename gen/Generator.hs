module Generator
    ( Generator(..)
    , generate
    ) where

data Generator a =
    Generator
        { imports :: [String]
        , toString :: a -> String
        }

generate :: String -> Generator a -> [a] -> String
generate m (Generator is ts) elts =
    header m ++
    "module " ++
    m ++
    "where\n\n" ++
    unlines (map (\i -> "import " ++ i) is) ++ "\n" ++ unlines (map (\e -> ts e ++ "\n") elts)

header :: String -> String
header m =
    "-- | \n\
    \-- Module:      " ++
    m ++
    " \n" ++
    "-- Copyright:   (c) 2019 Cedric Liegeois \n\
    \-- License:     BSD3 \n\
    \-- Maintainer:  Cedric Liegeois <ofmooseandmen@yahoo.fr> \n\
    \-- Stability:   experimental \n\
    \-- Portability: portable \n\
    \-- \n"