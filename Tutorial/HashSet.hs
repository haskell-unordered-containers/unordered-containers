{-# OPTIONS_GHC -fno-warn-unused-imports #-}

{-|
Sets allow you to store *unique* elements, providing efficient insertion,
lookups, and deletions. If you are storing sets of @Int@ s consider using
'Data.IntSet' from the <https://hackage.haskell.org/packages/containers containers> package. You can find the
introductory documentation for @containers@ at
<https://haskell-containers.readthedocs.io>.

@
data HashSet element = ...
@


All of these implementations are *immutable* which means that any update
functions do not modify the set that you passed in, they creates a new set. In
order to keep the changes you need to assign it to a new variable. For example:

@
let s1 = HashSet.fromList ["a", "b"]
let s2 = HashSet.delete "a" s1
print s1
> fromList ["a","b"]
print s2
> fromList ["b"]
@

__IMPORTANT:__ @HashSet@ relies on the @element@ type having instances of the @Eq@ and
   @Hashable@ typeclasses for its internal representation. These are already
   defined for builtin types, and if you are using your own data type you can
   use the
   <https://en.wikibooks.org/wiki/Haskell/Classes_and_types#Deriving deriving>
   mechanism.


-}

module Tutorial.HashSet (
    -- * Short Example
    -- $shortexample

                        ) where


{- $shortexample

The following GHCi session shows some of the basic set functionality:

@
import qualified Data.HashSet as HashSet

let dataStructures = HashSet.fromList ["HashSet", "HashMap", "Graph"]

-- Check if "HashMap" and "Trie" are in the set of data structures.
HashSet.member "HashMap" dataStructures
> True

HashSet.member "Trie" dataStructures
> False


-- Add "Trie" to our original set of data structures.
let moreDataStructures = HashSet.insert "Trie" dataStructures

HashSet.member "Trie" moreDataStructures
> True


-- Remove "Graph" from our original set of data structures.
let fewerDataStructures = HashSet.delete "Graph" dataStructures

HashSet.toList fewerDataStructures
> ["HashSet", "HashMap"]


-- Create a new set and combine it with our original set.
let orderedDataStructures = HashSet.fromList ["Set", "Map"]

HashSet.union dataStructures orderedDataStructures
> fromList ["Map", "HashSet", "Graph", "HashMap", "Set"]
@


__TIP__: You can use the `OverloadedLists
	 <https://ghc.haskell.org/trac/ghc/wiki/OverloadedLists>`_ extension so
	 you don't need to write ``fromList [1, 2, 3]`` everywhere. Instead you
	 can just write ``[1, 2, 3]`` and if the function is expecting a set it
	 will be converted automatically! The code here will continue to use
	 ``fromList`` for clarity though.


-}
