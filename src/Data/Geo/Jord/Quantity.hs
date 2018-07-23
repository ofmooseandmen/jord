{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module:      Data.Geo.Jord.Quantity
-- Copyright:   (c) 2018 Cedric Liegeois
-- License:     BSD3
-- Maintainer:  Cedric Liegeois <ofmooseandmen@yahoo.fr>
-- Stability:   experimental
-- Portability: portable
--
-- Classes for working with quantities.
--
module Data.Geo.Jord.Quantity
    ( Norm(..)
    , Quantity(..)
    ) where

-- | Norm of a vector: stricly positive _size_ or _length_ of vectorr @a@.
class Norm a b where
  norm :: a -> b

-- | Something that can be added or subtracted.
class (Eq a) => Quantity a where
    add, sub :: a -> a -> a
    zero :: a -- ^ identity
    isZero :: a -> Bool -- ^ is identity ?
    isZero a = a == zero
