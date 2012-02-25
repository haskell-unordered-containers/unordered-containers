{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module Data.HashMap.PopCount
    ( popCount
    ) where

import Data.Word (Word)

#if __GLASGOW_HASKELL__ >= 702
import Foreign.C (CUInt(..))
#else
import Foreign.C (CUInt)
#endif

foreign import ccall unsafe "popc.h popcount" c_popcount :: CUInt -> CUInt

popCount :: Word -> Int
popCount w = fromIntegral (c_popcount (fromIntegral w))
