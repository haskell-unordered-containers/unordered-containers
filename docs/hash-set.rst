Hash Sets
=========

.. highlight:: haskell

Sets allow you to store *unique* elements, providing efficient insertion,
lookups, and deletions. If you are storing sets of ``Int`` s consider using
``Data.IntSet`` from the :haddock:`containers` package. You can find the
introductory documentation for `containers` at
https://haskell-containers.readthedocs.io.

::

    data HashSet element = ...

.. IMPORTANT::
   ``HashSet`` relies on the `element` type having instances of the ``Eq`` and
   ``Hashable`` typeclasses for its internal representation. These are already
   defined for builtin types, and if you are using your own data type you can
   use the `deriving
   <https://en.wikibooks.org/wiki/Haskell/Classes_and_types#Deriving>`_
   mechanism.


All of these implementations are *immutable* which means that any update
functions do not modify the set that you passed in, they creates a new set. In
order to keep the changes you need to assign it to a new variable. For example::

    let s1 = HashSet.fromList ["a", "b"]
    let s2 = HashSet.delete "a" s1
    print s1
    > fromList ["a","b"]
    print s2
    > fromList ["b"]


Short Example
-------------

The following GHCi session shows some of the basic set functionality::

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



.. TIP:: You can use the `OverloadedLists
	 <https://ghc.haskell.org/trac/ghc/wiki/OverloadedLists>`_ extension so
	 you don't need to write ``fromList [1, 2, 3]`` everywhere. Instead you
	 can just write ``[1, 2, 3]`` and if the function is expecting a set it
	 will be converted automatically! The code here will continue to use
	 ``fromList`` for clarity though.


Importing HashSet
-----------------

When using ``HashSet`` in a Haskell source file you should always use a
``qualified`` import because these modules export names that clash with the
standard Prelude. You can import the type constructor unqualified.

::

    import Data.HashSet (HashSet)
    import qualified Data.HashSet as HashSet


Common API Functions
--------------------

.. TIP::
   All of these functions that work for ``HashSet`` will also work for
   ``IntSet``, which has the element type ``a`` specialized to ``Int``. Anywhere
   that you see ``HashSet Int`` you can replace it with ``IntSet``. This will
   speed up most operations tremendously (see `Performance`_) with the exception
   of ``size`` which is O(1) for ``HashSet`` and O(n) for ``IntSet``.

.. NOTE::
   ``fromList [some,list,elements]`` is how a ``HashSet`` is printed.


Construction and Conversion
^^^^^^^^^^^^^^^^^^^^^^^^^^^

Create an empty set
"""""""""""""""""""

::

    HashSet.empty :: HashSet a
    HashSet.empty = ...

:haddock_short:`/Data.HashSet#empty` creates a set with zero elements.

::

    HashSet.empty
    > fromList []

Create a set with one element (singleton)
"""""""""""""""""""""""""""""""""""""""""

::

    HashSet.singleton :: a -> HashSet a
    HashSet.singleton x = ...

:haddock_short:`/Data.HashSet#singleton` creates a set with a single element
``x`` in it.

::

    HashSet.singleton "containers"
    > fromList ["containers"]

    HashSet.singleton 1
    > fromList [1]

Create a set from a list
""""""""""""""""""""""""

::

    HashSet.fromList :: [a] -> HashSet a
    HashSet.fromList xs = ...

:haddock_short:`/Data.HashSet#fromList` creates a set containing the elements of the
list ``xs``. Since sets don't contain duplicates, if there are repeated elements
in the list they will only appear once.

::

    HashSet.fromList ["base", "containers", "QuickCheck"]
    > fromList [,"containers","base","QuickCheck"]

    HashSet.fromList [1, 1, 2, 3, 4, 4, 5, 1]
    > fromList [1,2,3,4,5]

Create a list from a set
""""""""""""""""""""""""

::

    HashSet.toList :: HashSet a -> [a]
    HashSet.toList s = ...

:haddock_short:`/Data.HashSet#toList` returns a list containing the elements of
the set, the order is unspecified.


Querying
^^^^^^^^

Check if an element is in a set (member)
""""""""""""""""""""""""""""""""""""""""

::

    HashSet.member :: a -> HashSet a -> Bool
    HashSet.member x s = ...

:haddock_short:`/Data.HashSet#member` returns ``True`` if the element ``x`` is
in the set ``s``, ``False`` otherwise.

::

    HashSet.member 0 HashSet.empty
    > False

    HashSet.member 0 (HashSet.fromList [0, 2, 4, 6])
    > True

Check if a set is empty
"""""""""""""""""""""""

::

    HashSet.null :: HashSet a -> Bool
    HashSet.null s = ...

:haddock_short:`/Data.HashSet#null` returns ``True`` if the set ``s`` is empty,
``False`` otherwise.

::

    HashSet.null HashSet.empty
    > True

    HashSet.null (HashSet.fromList [0, 2, 4, 6])
    > False


The number of elements in a set
"""""""""""""""""""""""""""""""

::

    HashSet.size :: HashSet a -> Int
    HashSet.size s = ...

:haddock_short:`/Data.HashSet#size` returns the number of elements in the set
``s``.

::

    HashSet.size HashSet.empty
    > 0

    HashSet.size (HashSet.fromList [0, 2, 4, 6])
    > 4


Modification
^^^^^^^^^^^^

Adding a new element to a set
"""""""""""""""""""""""""""""

::

    HashSet.insert :: a -> HashSet a -> HashSet a
    HashSet.insert x s = ...

:haddock_short:`/Data.HashSet#insert` places the element ``x`` into the set
``s``, replacing an existing equal element if it already exists.

::

    HashSet.insert 100 HashSet.empty
    > fromList [100]

    HashSet.insert 0 (HashSet.fromList [0, 2, 4, 6])
    > fromList [0,2,4,6]

Removing an element from a set
""""""""""""""""""""""""""""""

::

    HashSet.delete :: a -> HashSet a -> HashSet a
    HashSet.delete x s = ...

:haddock_short:`/Data.HashSet#delete` the element ``x`` from the set ``s``. If
it’s not a member it leaves the set unchanged.

::

    HashSet.delete 0 (HashSet.fromList [0, 2, 4, 6])
    > fromList [2,4,6]

Filtering elements from a set
"""""""""""""""""""""""""""""

::

    HashSet.filter :: (a -> Bool) -> HashSet a -> HashSet a
    HashSet.filter predicate s = ...

:haddock_short:`/Data.HashSet#filter` produces a set consisting of all elements
of ``s`` for which the ``predicate`` returns ``True``.

::

    HashSet.filter (==0) (HashSet.fromList [0, 2, 4, 6])
    > fromList [0]


Set Operations
^^^^^^^^^^^^^^

Union
"""""

::

    HashSet.union :: HashSet a -> HashSet a -> HashSet a
    HashSet.union l r = ...

:haddock_short:`/Data.HashSet#union` returns a set containing all elements that
are in either of the two sets ``l`` or ``r`` (`set union
<https://en.wikipedia.org/wiki/Union_(set_theory)>`_).

::

    HashSet.union HashSet.empty (HashSet.fromList [0, 2, 4, 6])
    > fromList [0,2,4,6]

    HashSet.union (HashSet.fromList [1, 3, 5, 7]) (HashSet.fromList [0, 2, 4, 6])
    > fromList [0,1,2,3,4,5,6,7]

Intersection
""""""""""""

::

    HashSet.intersection :: HashSet a -> HashSet a -> HashSet a
    HashSet.intersection l r = ...

:haddock_short:`/Data.HashSet#intersection` returns a set the elements that are
in both sets ``l`` and ``r`` (`set intersection
<https://en.wikipedia.org/wiki/Intersection_(set_theory)>`_).

::

    HashSet.intersection HashSet.empty (HashSet.fromList [0, 2, 4, 6])
    > fromList []

    HashSet.intersection (HashSet.fromList [1, 3, 5, 7]) (HashSet.fromList [0, 2, 4, 6])
    > fromList []

    HashSet.intersection (HashSet.singleton 0) (HashSet.fromList [0, 2, 4, 6])
    > fromList [0]

Difference
""""""""""

::

    HashSet.difference :: HashSet a -> HashSet a -> HashSet a
    HashSet.difference l r = ...

:haddock_short:`/Data.HashSet#difference` returns a set containing the elements
that are in the first set ``l`` but not the second set ``r`` (`set
difference/relative compliment
<https://en.wikipedia.org/wiki/Complement_(set_theory)#Relative_complement>`_).

::

    HashSet.difference (HashSet.fromList [0, 2, 4, 6]) HashSet.empty
    > fromList [0,2,4,6]

    HashSet.difference (HashSet.fromList [0, 2, 4, 6]) (HashSet.fromList [1, 3, 5, 7])
    > fromList [0,2,4,6]

    HashSet.difference (HashSet.fromList [0, 2, 4, 6]) (HashSet.singleton 0)
    > fromList [2,4,6]


Performance
-----------

The API docs are annotated with the Big-*O* complexities of each of the set
operations. For benchmarks see the `haskell-perf/sets
<https://github.com/haskell-perf/sets>`_ page.


Looking for more?
-----------------

Didn't find what you're looking for? This tutorial only covered the most common
set functions, for a full list of functions see the
:haddock_short:`/Data.HashSet#HashSet` documentation.
