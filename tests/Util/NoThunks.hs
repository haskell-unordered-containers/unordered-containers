{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Orphan 'NoThunks' instances for 'HashMap' and 'HashSet' that traverse
-- the internal tree structure (not just the values reachable via
-- 'Foldable'), so that they can catch thunks left behind in the tree itself,
-- e.g. #232.
module Util.NoThunks (noThunksProperty) where

import Data.HashMap.Internal       (HashMap (..), Leaf (..))
import Data.HashSet                (HashSet)
import NoThunks.Class              (NoThunks (..), OnlyCheckWhnf (..),
                                    ThunkInfo, allNoThunks, noThunksInValues,
                                    unsafeNoThunks)
import Test.QuickCheck             (Property, counterexample, property)
import Util.Key                    (Key)

import qualified Data.HashMap.Internal.Array as A
import qualified Data.HashSet                as HS

instance (NoThunks k, NoThunks v) => NoThunks (HashMap k v) where
  showTypeOf _ = "HashMap"
  wNoThunks ctxt m = case m of
    Empty              -> return Nothing
    BitmapIndexed _ ary -> noThunksInValues ctxt (A.toList ary)
    Leaf _ l            -> noThunks ctxt l
    Full ary             -> noThunksInValues ctxt (A.toList ary)
    Collision _ ary      -> noThunksInValues ctxt (A.toList ary)

instance (NoThunks k, NoThunks v) => NoThunks (Leaf k v) where
  showTypeOf _ = "Leaf"
  wNoThunks ctxt (L k v) = allNoThunks [noThunks ctxt k, noThunks ctxt v]

instance NoThunks a => NoThunks (HashSet a) where
  showTypeOf _ = "HashSet"
  wNoThunks ctxt = wNoThunks ctxt . HS.toMap

-- | 'Key' has only strict, flat fields, so checking WHNF suffices; this
-- avoids depending on 'Util.Key' exporting the 'SmallSum' constructors.
deriving via OnlyCheckWhnf Key instance NoThunks Key

-- | Check a value for unexpected thunks, reporting the 'ThunkInfo' as a
-- QuickCheck counterexample on failure.
--
-- Forces @x@ to WHNF first: 'noThunks' only checks whether its argument
-- itself is a thunk before recursing into it, so without this an
-- as-yet-unevaluated @x@ (e.g. a fresh @let@ binding) would always be
-- reported as "a thunk", which isn't the structural invariant we're after.
noThunksProperty :: NoThunks a => a -> Property
noThunksProperty x = x `seq` case unsafeNoThunks x of
  Nothing -> property True
  Just info -> counterexample (show (info :: ThunkInfo)) (property False)
