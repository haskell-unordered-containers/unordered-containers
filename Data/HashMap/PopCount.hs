{-# LANGUAGE ForeignFunctionInterface #-}

module Data.HashMap.PopCount
    ( popCount
    ) where

import Data.Word (Word)
import Foreign.C (CUInt)

foreign import ccall unsafe "popc.h popcount" c_popcount :: CUInt -> CUInt

popCount :: Word -> Int
popCount w = fromIntegral (c_popcount (fromIntegral w))
