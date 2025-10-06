{-# language BangPatterns #-}

module Key.SlowInt (SlowInt(..)) where

import Data.Hashable (Hashable(..))
import Data.Bits ((.|.), (.&.), shiftR, finiteBitSize)

-- | @Int@s with slow `Eq` and `Hashable` instances, and a lowish possibility of
-- hash collisions.
newtype SlowInt = Slow { unSlow :: Int }
  deriving (Ord, Show, Read)

instance Eq SlowInt where
  Slow a == Slow b = slow a == slow b && a == b

instance Hashable SlowInt where
  hashWithSalt salt (Slow n) = hashWithSalt (hashWithSalt salt (slow n')) n'
    where
      -- We use only the lower 20 bits to increase the chance of hash collisions
      n' = n .&. 0xfffff

slowFib :: Int -> Int
slowFib n = go n 0 1
  where
    go 0 !a _ = a
    go k !a !b = go (k - 1) b (a + b)

lowBits :: Int -> Int
lowBits n = n .&. 0xff

slow :: Int -> Int
slow = slowFib . lowBits
