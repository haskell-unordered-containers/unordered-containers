{-# OPTIONS_GHC -fno-warn-unused-imports #-}

{-|
Sets allow you to store *unique* elements, providing efficient insertion,
lookups, and deletions. If you are storing sets of @Int@ s consider using
'Data.IntSet' from the <https://hackage.haskell.org/packages/containers containers> package. You can find the
introductory documentation for @containers@ at
<https://haskell-containers.readthedocs.io>.

@
data HashMap k v = ...
@
-}

module Tutorial.HashMap () where
