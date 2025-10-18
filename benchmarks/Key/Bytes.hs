{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Key.Bytes where

import Control.DeepSeq
import Control.Monad (replicateM)
import Data.ByteString.Short
import Data.Hashable
import Data.List
import System.Random.Stateful

newtype Bytes = Bytes {unBytes :: ShortByteString}
  deriving (Eq, Hashable, Show, NFData)

genBytes ::
  (StatefulGen g m) =>
  Int ->
  g ->
  m Bytes
genBytes len gen = Bytes <$> uniformShortByteStringM len gen

genNBytes ::
  (StatefulGen g m) =>
  Int ->
  Int ->
  g ->
  m [Bytes]
genNBytes n len = replicateM n . genBytes len

-- | @genDisjoint n len gen@ generates @n@ 'Bytes' in total. The returned lists
-- each contain roughly half of the total.
genDisjoint ::
  (StatefulGen g m) =>
  Int ->
  -- | Must be positive
  Int ->
  g ->
  m ([Bytes], [Bytes])
genDisjoint n len gen = Data.List.partition predicate <$> genNBytes n len gen
  where
    predicate (Bytes sbs) = even (Data.ByteString.Short.head sbs)

{-
instance Uniform Bytes where
  uniformM = genBytes 32
-}
