{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE MagicHash        #-}
{-# LANGUAGE TypeApplications #-}

module Util.Key (Key(..), keyToInt, incKey, collisionAtHash) where

import Data.Bits       (bit, (.&.))
import Data.Hashable   (Hashable (hashWithSalt))
import Data.Word       (Word16)
import GHC.Exts        (Int (..), bitReverse#, int2Word#, word2Int#)
import GHC.Generics    (Generic)
import Test.QuickCheck (Arbitrary (..), CoArbitrary (..), Function, Gen, Large)

import qualified Test.QuickCheck as QC

-- Key type that generates more hash collisions.
data Key = K
  { hash :: !Int
    -- ^ The hash of the key
  , _x :: !SmallSum
    -- ^ Additional data, so we can have collisions for any hash
  } deriving (Eq, Ord, Read, Show, Generic, Function, CoArbitrary)

instance Hashable Key where
  hashWithSalt _ (K h _) = h

data SmallSum = A | B | C | D
  deriving (Eq, Ord, Read, Show, Generic, Enum, Bounded, Function, CoArbitrary)

instance Arbitrary SmallSum where
  arbitrary = QC.arbitraryBoundedEnum
  shrink = shrinkSmallSum

shrinkSmallSum :: SmallSum -> [SmallSum]
shrinkSmallSum A = []
shrinkSmallSum B = [A]
shrinkSmallSum C = [A, B]
shrinkSmallSum D = [A, B, C]

instance Arbitrary Key where
  arbitrary = K <$> arbitraryHash <*> arbitrary
  shrink = QC.genericShrink

arbitraryHash :: Gen Int
arbitraryHash = do
  let gens =
        [ (2, fromIntegral . QC.getLarge <$> arbitrary @(Large Word16))
        , (1, QC.getSmall <$> arbitrary)
        , (1, QC.getLarge <$> arbitrary)
          -- Hashes where the lowest `maxChildren` bits are set are interesting
          -- edge cases. See #491.
        , (1, QC.elements [-1, 0xFF, 0xFFF])
        ]
  i <- QC.frequency gens
  transform <- QC.elements [id, moreCollisions, bitReverse]
  pure (transform i)

-- | Mask out most bits to produce more collisions
moreCollisions :: Int -> Int
moreCollisions w = fromIntegral (w .&. moreCollisionsMask)

-- | Bitmask for @moreCollisions@
moreCollisionsMask :: Int
moreCollisionsMask = sum [bit n | n <- [0, 3, 8, 14, 61]]

-- | Reverse order of bits, in order to generate variation in the
-- high bits, resulting in HashMap trees of greater height.
bitReverse :: Int -> Int
bitReverse (I# i) = I# (word2Int# (bitReverse# (int2Word# i)))

keyToInt :: Key -> Int
keyToInt (K h x) = h * fromEnum x

incKey :: Key -> Key
incKey (K h x) = K (h + 1) x

-- | 4 colliding keys at a given hash.
collisionAtHash :: Int -> (Key, Key, Key, Key)
collisionAtHash h = (K h A, K h B, K h C, K h D)
