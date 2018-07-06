module Data.Geo.Jord.Expr.AST
    ( Node(..)
    , Func(..)
    , parse
    ) where

import qualified Data.Geo.Jord.Expr.Token as T

data Node = Call Func | Lit String deriving (Eq, Show)

data Func = Func
   { name :: String
   , params :: [Node]
   } deriving (Eq, Show)

-- | syntax is (f x y) where x and y can be function themselves.
parse :: [T.Token] -> Node
parse = walk

walk :: [T.Token] -> Node
walk ts =
    case head ts of
        (T.Str n) -> Lit n
        (T.Paren '(') -> walk (tail ts)
        _ -> error "TODO"
