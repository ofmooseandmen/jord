-- |
-- Module:      Data.Geo.Jord.Quantity
-- Copyright:   (c) 2018 Cedric Liegeois
-- License:     BSD3
-- Maintainer:  Cedric Liegeois <ofmooseandmen@yahoo.fr>
-- Stability:   experimental
-- Portability: portable
--
-- Defines the class 'Quantity' for something that can be added or subtracted.
--
module Data.Geo.Jord.Quantity
    ( Quantity(..)
    ) where

-- | Something that can be added or subtracted.
class (Eq a) => Quantity a where
    add, sub :: a -> a -> a
    zero :: a -- ^ identity
    isZero :: a -> Bool -- ^ is identity ?
    isZero a = a == zero
