{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Test utilities
module Utils
    ( Key (..)
    , Expr (..)
    , evalExprStrict
    , evalExprLazy
    , evalExprSet
    , evalExprOrdSet
    ) where

import Control.Applicative (liftA2, liftA3)
import Data.Hashable (Hashable(hashWithSalt))
import Test.QuickCheck (Arbitrary(arbitrary,shrink), oneof, sized)

import qualified Data.HashMap.Strict as HMS
import qualified Data.HashMap.Lazy as HML
import qualified Data.HashSet as HS
import qualified Data.Set as Set

-- | A key with collision-prone Hashable instance
------------------------------------------------------------------------

-- Key type that generates more hash collisions.
newtype Key = K { unK :: Int }
   deriving (Arbitrary, Enum, Eq, Ord, Read, Show, Integral, Num, Real)

instance Hashable Key where
    hashWithSalt salt k = hashWithSalt salt (unK k) `mod` 20

-- | A HashMap/HashSet building expression
------------------------------------------------------------------------

data Expr k v
    = ExprEmpty
    | ExprSingleton k v
    | ExprInsert k v (Expr k v)
    | ExprDelete k (Expr k v)
    | ExprUnion (Expr k v) (Expr k v)
  deriving Show

instance (Arbitrary k, Arbitrary v) => Arbitrary (Expr k v) where
    arbitrary = sized arb
      where
        arb n | n <= 0 = oneof leafs
        arb n = oneof $ leafs ++
            [ liftA3 ExprInsert arbitrary arbitrary (arb (n - 1))
            , liftA2 ExprDelete arbitrary (arb (n - 1))
            , liftA2 ExprUnion (arb (n `div` 2)) (arb (n - n `div` 2))
            ]

        leafs =
            [ return ExprEmpty
            , liftA2 ExprSingleton arbitrary arbitrary
            ]

    shrink ExprEmpty
        = []
    shrink (ExprSingleton k v) =
        ExprEmpty : uncurry ExprSingleton `map` shrink (k, v)
    shrink (ExprInsert k v e) =
        ExprEmpty : e : uncurry3 ExprInsert `map` shrink (k, v, e)
    shrink (ExprDelete k e) =
        ExprEmpty : e : uncurry ExprDelete `map` shrink (k, e)
    shrink (ExprUnion a b) =
        ExprEmpty : a : b : uncurry ExprUnion `map` shrink (a, b)

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

evalExprStrict :: (Eq k, Hashable k) => Expr k v -> HMS.HashMap k v
evalExprStrict = go
  where
    go ExprEmpty           = HMS.empty
    go (ExprSingleton k v) = HMS.singleton k v
    go (ExprInsert k v e)  = HMS.insert k v (go e)
    go (ExprDelete k e)    = HMS.delete k (go e)
    go (ExprUnion a b)     = HMS.union (go a) (go b)

evalExprLazy :: (Eq k, Hashable k) => Expr k v -> HML.HashMap k v
evalExprLazy = go
  where
    go ExprEmpty           = HML.empty
    go (ExprSingleton k v) = HML.singleton k v
    go (ExprInsert k v e)  = HML.insert k v (go e)
    go (ExprDelete k e)    = HML.delete k (go e)
    go (ExprUnion a b)     = HML.union (go a) (go b)

evalExprSet :: (Eq k, Hashable k) => Expr k () -> HS.HashSet k
evalExprSet = go
  where
    go ExprEmpty           = HS.empty
    go (ExprSingleton k _) = HS.singleton k
    go (ExprInsert k _ e)  = HS.insert k (go e)
    go (ExprDelete k e)    = HS.delete k (go e)
    go (ExprUnion a b)     = HS.union (go a) (go b)

evalExprOrdSet :: (Ord k) => Expr k () -> Set.Set k
evalExprOrdSet = go
  where
    go ExprEmpty           = Set.empty
    go (ExprSingleton k _) = Set.singleton k
    go (ExprInsert k _ e)  = Set.insert k (go e)
    go (ExprDelete k e)    = Set.delete k (go e)
    go (ExprUnion a b)     = Set.union (go a) (go b)
