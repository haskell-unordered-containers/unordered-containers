{-# OPTIONS_GHC -fno-warn-unused-imports #-}

{-|
    The @unordered-containers@ package provides implementations of various
    hash-based immutable data structures.

    Some of the data structures provided by this package have a very large API
    surface (for better or worse). The docs here focus on the most common functions
    which should be more than enough to get you started. Once you know the basics,
    or if you're looking for a specific function, you can head over to the
    full API documentation!
-}


module Tutorial (
    -- * Provided Data Structures
    -- $provideddatastructures

    -- * Related Packages
    -- $relatedpackages

    -- * Looking for more resources?
    -- $moreresources

    -- * Installing and using the @unordered-containers@ packages
    -- $installing

    -- * HashSet and HashMap tutorial
    -- $tutorials

                ) where

{- $provideddatastructures
* 'Data.HashSet' - unordered, non-duplicated elements
* 'Data.HashMap' - unordered map from keys to values (aka. dictionaries)
-}

{- $relatedpackages
* <https://hackage.haskell.org/packages/containers containers> - ordered containers using trees instead of hashing.
* <https://hackage.haskell.org/packages/hashable containers> - types that can be converted to a hash value.
-}

{- $moreresources
If you've worked your way through the documentation here and you're looking for
more examples or tutorials you should check out:

* <https://haskell-lang.org/library/containers haskell-lang.org's containers tutorial>, its focused on the ordered
  <https://hackage.haskell.org/packages/containers containers> library but provides some useful examples.
* <http://learnyouahaskell.com/modules Learn You a Haskell "Modules" chapter>
-}

{- $installing
__Version Requirements__

All of the examples here should work for all recent versions of the package.

__Importing modules__

All of the modules in @unordered-containers@@ should be imported @qualified@
since they use names that conflict with the standard Prelude.

@
import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HashMap
@

__In GHCi__

Start the GHCi
<https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop REPL> with
@ghci@, @cabal repl@, or @stack ghci@. Once the REPL is loaded, import the
modules you want using the @import@ statements above and you're good to go!
-}


{- $tutorials

See "Tutorial.HashSet" and "Tutorial.HashMap".

-}
