{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Key.Bytes where

import Control.Monad (replicateM)
import Data.ByteString.Short
import Data.Hashable
import System.Random.Stateful
import Control.DeepSeq

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

genDisjoint ::
  (StatefulGen g m) =>
  Int ->
  Int ->
  g ->
  m ([Bytes], [Bytes])
genDisjoint n len = undefined
{-
instance Uniform Bytes where
  uniformM = genBytes 32 
-}
