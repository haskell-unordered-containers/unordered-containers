{-# LANGUAGE ForeignFunctionInterface #-}

module Data.HashMap.PopCount
    ( popCount
    ) where

import Data.Word
import Foreign.C

foreign import ccall unsafe "popc.h popcount" c_popcount :: CUInt -> CUInt

popCount :: Word -> Int
popCount w = fromIntegral (c_popcount (fromIntegral w))

