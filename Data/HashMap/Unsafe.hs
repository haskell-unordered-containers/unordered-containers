{-# LANGUAGE MagicHash, Rank2Types, UnboxedTuples #-}

-- | This module exports a workaround for this bug:
--
--    http://hackage.haskell.org/trac/ghc/ticket/5916
--
-- Please read the comments in ghc/libraries/base/GHC/ST.lhs to
-- understand what's going on here.
--
-- Code that uses this module should be compiled with -fno-full-laziness
module Data.HashMap.Unsafe
    ( runST
    ) where

import GHC.Base (realWorld#)
import qualified GHC.ST as ST

-- | Return the value computed by a state transformer computation.
-- The @forall@ ensures that the internal state used by the 'ST'
-- computation is inaccessible to the rest of the program.
runST :: (forall s. ST.ST s a) -> a
runST st = runSTRep (case st of { ST.ST st_rep -> st_rep })
{-# INLINE runST #-}

runSTRep :: (forall s. ST.STRep s a) -> a
runSTRep st_rep = case st_rep realWorld# of
                        (# _, r #) -> r
{-# INLINE [0] runSTRep #-}
