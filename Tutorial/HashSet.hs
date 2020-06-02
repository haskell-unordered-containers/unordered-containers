{-# OPTIONS_GHC -fno-warn-unused-imports #-}

{-|
Sets allow you to store *unique* elements, providing efficient insertion,
lookups, and deletions. If you are storing sets of @Int@ s consider using
'Data.IntSet' from the <https://hackage.haskell.org/packages/containers containers> package. You can find the
introductory documentation for @containers@ at
<https://haskell-containers.readthedocs.io>.

@
data 'HashSet' element = ...
@


All of these implementations are /immutable/ which means that any update
functions do not modify the set that you passed in, they creates a new set. In
order to keep the changes you need to assign it to a new variable. For example:

@
import qualified Data.HashSet as HashSet

let s1 = HashSet.'HashSet.fromList' ["a", "b"]
let s2 = HashSet.'HashSet.delete' "a" s1
print s1
> HashSet.'HashSet.fromList' ["a","b"]
print s2
> HashSet.'HashSet.fromList' ["b"]
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

    -- * Importing HashSet
    -- $importing

    -- * Common API Functions
    -- ** Construction and Conversion

    -- *** Create an empty set
    -- $empty

    -- *** Create a set with one element (singleton)
    -- $singleton

    -- *** Create a set from a list
    -- $fromlist

    -- * Continue reading
    -- $continuereading
                        ) where

import qualified Data.HashSet as HashSet

{- $shortexample

The following GHCi session shows some of the basic set functionality:

@
import qualified Data.HashSet as HashSet

let dataStructures = HashSet.'HashSet.fromList' [\"HashSet\", \"HashMap\", \"Graph\"]

-- Check if HashMap and Trie are in the set of data structures.
HashSet.'HashSet.member' \"HashMap\" dataStructures
> True

HashSet.'HashSet.member' "Trie" dataStructures
> False


-- Add Trie to our original set of data structures.
let moreDataStructures = HashSet.'HashSet.insert' \"Trie\" dataStructures

HashSet.'HashSet.member' \"Trie\" moreDataStructures
> True


-- Remove Graph from our original set of data structures.
let fewerDataStructures = HashSet.'HashSet.delete' \"Graph\" dataStructures

HashSet.'HashSet.toList' fewerDataStructures
> [\"HashSet\", \"HashMap\"]


-- Create a new set and combine it with our original set.
let orderedDataStructures = HashSet.'HashSet.fromList' [\"Set\", \"Map\"]

HashSet.'HashSet.union' dataStructures orderedDataStructures
> fromList [\"Map\", \"HashSet\", \"Graph\", \"HashMap\", \"Set\"]
@


__TIP__: You can use the
         <https://ghc.haskell.org/trac/ghc/wiki/OverloadedLists OverloadedLists>
         extension so you don't need to write `fromList [1, 2, 3]` everywhere.
         Instead you can just write `[1, 2, 3]` and if the function is expecting
         a set it will be converted automatically! The code here will continue
         to use `fromList` for clarity though.


-}


{- $importing

When using HashSet in a Haskell source file you should always use a `qualified`
import because these modules export names that clash with the standard Prelude.
You can import the type constructor unqualified.

@
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
@

-}

{- $commonapifunctions


.. TIP::
   All of these functions that work for `HashSet` will also work for
   `IntSet`, which has the element type `a` specialized to `Int`. Anywhere
   that you see `HashSet Int` you can replace it with `IntSet`. This will
   speed up most operations tremendously (see `Performance`_) with the exception
   of `size` which is O(1) for `HashSet` and O(n) for `IntSet`.

.. NOTE::
   `fromList [some,list,elements]` is how a `HashSet` is printed.

-}

{- $empty
@
HashSet.empty :: HashSet a
HashSet.empty = ...
@

`HashSet.empty' creates a set with zero elements.

@
HashSet.empty
> fromList []
@

-}

{- $singleton
@
HashSet.singleton :: a -> HashSet a
HashSet.singleton x = ...
@

'HashSet.singleton' creates a set with a single element
`x` in it.

@
HashSet.singleton "containers"
> fromList ["containers"]

HashSet.singleton 1
> fromList [1]
@
-}

{- $fromlist
@
HashSet.fromList :: [a] -> HashSet a
HashSet.fromList xs = ...
@

'HashSet.fromList' creates a set containing the elements of the
list `xs`. Since sets don't contain duplicates, if there are repeated elements
in the list they will only appear once.

@
HashSet.fromList ["base", "containers", "QuickCheck"]
> fromList [,"containers","base","QuickCheck"]

HashSet.fromList [1, 1, 2, 3, 4, 4, 5, 1]
> fromList [1,2,3,4,5]
@
-}

{- $continuereading
Continue the tutorial at "Tutorial.HashMap".
-}
