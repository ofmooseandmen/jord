-- |
-- Module:      Data.Geo.Jord.Quantity
-- Copyright:   (c) 2019 Cedric Liegeois
-- License:     BSD3
-- Maintainer:  Cedric Liegeois <ofmooseandmen@yahoo.fr>
-- Stability:   experimental
-- Portability: portable
--
-- Classes for working with quantities.
--
module Data.Geo.Jord.Quantity
    ( Quantity(..)
    ) where

-- | Something that can be added or subtracted.
class (Eq a, Ord a) =>
      Quantity a
    where
    add, sub :: a -> a -> a
    zero :: a
