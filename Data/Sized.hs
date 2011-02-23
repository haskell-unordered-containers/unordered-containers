module Data.Sized
    ( Sized(..)
    ) where

-- | A pair of a value and some measure of its size (either absolute
-- or a delta).
data Sized a = {-# UNPACK #-} !Int :!: !a
              deriving (Eq, Ord, Show, Read, Bounded)

infixl 2 :!:
