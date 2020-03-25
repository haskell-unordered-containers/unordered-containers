Hash Maps
=========

.. highlight:: haskell

Maps (sometimes referred to as dictionaries in other languages) allow you to
store associations between *unique keys* and *values*. There are two
implementations provided by the ``unordered-containers`` package:
:haddock:`/Data.HashMap.Strict` and :haddock:`/Data.HashMap.Lazy`. You almost
never want the lazy version so use ``Data.Map.Strict``, and if your keys are
``Int`` consider using ``Data.IntMap`` from the :haddock:`containers` package
which is `faster <https://github.com/haskell-perf/dictionaries>`_ for many
operations.

::

    data HashMap k v = ...

.. IMPORTANT::
   ``HashMap`` relies on the key type ``k`` having instances of the ``Eq`` and
   ``Hashable`` typeclasses for its internal representation. These are already
   defined for builtin types, and if you are using your own data type you can
   use the `deriving
   <https://en.wikibooks.org/wiki/Haskell/Classes_and_types#Deriving>`_
   mechanism.

All of these implementations are *immutable* which means that any update
functions do not modify the map that you passed in, they creates a new map. In
order to keep the changes you need to assign it to a new variable. For example::

    let m1 = HashMap.fromList [("a", 1), ("b", 2)]
    let m2 = HashMap.delete "a" m1
    print m1
    > fromList [("a",1),("b",2)]
    print m2
    > fromList [("b",2)]


Short Example
-------------

The following GHCi session shows some of the basic hash map functionality::

    import qualified Data.HashMap.Strict as HashMap

    let nums = HashMap.fromList [(1,"one"), (2,"two"), (3,"three")]

    -- Get the English word for the number 3 and 4.
    HashMap.lookup 3 nums
    > Just "three"

    HashMap.lookup 4 nums
    > Nothing


    -- Add (4, "four") to our original map.
    let moreNums = HashMap.insert 4 "four" nums

    HashMap.member 4 moreNums
    > True


    -- Remove the entry for 1 from our original map.
    let fewerNums = HashMap.delete 1 nums

    HashMap.toList fewerNums
    > [(2,"two"),(3,"three")]


    -- Create a new map and combine it with our original map.
    -- fromList is right-biased: if a key is repeated the rightmost value is taken.
    let newNums = HashMap.fromList [(3,"new three"), (4,"new four"), (4,"newer four")]

    -- union is left-biased: if a key occurs more than once the value from the
    -- left map is taken.
    HashMap.union newNums nums
    > fromList [(1,"one"),(2,"two"),(3,"new three"),(4,"newer four")]

.. TIP:: You can use the `OverloadedLists
	 <https://ghc.haskell.org/trac/ghc/wiki/OverloadedLists>`_ extension so
	 you don't need to write ``fromList [1, 2, 3]`` everywhere; instead you
	 can just write ``[1, 2, 3]`` and if the function is expecting a map it
	 will be converted automatically! The code here will continue to use
	 ``fromList`` for clarity though.


Importing HashMap
-----------------

When using ``HashMap`` in a Haskell source file you should always use a
``qualified`` import because these modules export names that clash with the
standard Prelude (you can import the type constructor on its own though!). You
should also import ``Prelude`` and hide ``lookup`` because if you accidentally
leave off the ``HashMap.`` qualifier you'll get confusing type errors. You can
always import any specific identifiers you want unqualified. Most of the time,
that will include the type constructor (``HashMap``).

::

    import Prelude hiding (lookup)

    import Data.HashMap.Strict (HashMap)
    import qualified Data.HashMap.Strict as HashMap


Common API Functions
--------------------

.. NOTE::
   A ``HashMap`` is printed as an association list preceeded by ``fromList``. For
   example, it might look like ``fromList [(Key1,True),(Key2,False)]``.


Construction and Conversion
^^^^^^^^^^^^^^^^^^^^^^^^^^^

Create an empty map
"""""""""""""""""""

::

    HashMap.empty :: HashMap k v
    HashMap.empty = ...

:haddock_short:`/Data.HashMap.Strict#empty` creates a map without any entries.

::

    HashMap.empty
    > fromList []

Create a map with one entry (singleton)
"""""""""""""""""""""""""""""""""""""""

::

    HashMap.singleton :: k -> v -> HashMap k v
    HashMap.singleton key value = ...

:haddock_short:`/Data.HashMap.Strict#singleton` creates a map with a single
``(key,value)`` entry in it.

::

    HashMap.singleton 1 "one"
    > fromList [(1,"one")]

    HashMap.singleton "containers" ["base"]
    > fromList [("containers",["base"])]

Create a map from a list
""""""""""""""""""""""""

::

    HashMap.fromList :: [(k, v)] -> HashMap k v
    HashMap.fromList xs = ...

:haddock_short:`/Data.HashMap.Strict#fromList` creates a map containing the entries
of the list ``xs`` where the keys comes from the first entries of the pairs and
the values from the second. If the same key appears more than once then the last
value is taken.

::

    HashMap.fromList []
    > fromList []

    HashMap.fromList [(1,"uno"), (1,"one"), (2,"two"), (3,"three")]
    > fromList [(1,"one"),(2,"two"),(3,"three")]

There's another incredibly useful function for constructing a map from a list::

    HashMap.fromListWith :: (a -> a -> a) -> [(k, a)] -> HashMap k a
    HashMap.fromListWith f xs = ...

:haddock_short:`/Data.HashMap.Strict#fromListWith` allows you to build a map from a
list ``xs`` with repeated keys, where ``f`` is used to "combine" (or "choose")
values with the same key.

::

    -- Build a map from a list, but only keep the largest value for each key.
    HashMap.fromListWith max [("a", 2), ("a", 1), ("b", 2)]
    > fromList [("a",2),("b",2)]

    -- Build a histogram from a list of elements.
    HashMap.fromListWith (+) (map (\x -> (x, 1)) ["a", "a", "b", "c", "c", "c"])
    > fromList [("a",2),("b",1),("c",3)]

    -- Build a map from a list, combining the string values for the same key.
    HashMap.fromListWith (++) [(1, "a"), (1, "b"), (2, "x"), (2, "y")]
    > fromList [(1,"ba"),(2,"yx")]



Create a list from a map
""""""""""""""""""""""""

::

    HashMap.toList :: HashMap k v -> [(k, v)]
    HashMap.toList m = ...

.. NOTE::
   ``HashMap.toList`` is **not** the same as ``Foldable.toList``; the latter is
   equivalent to ``elems``, although is rarely useful for maps.

:haddock_short:`/Data.HashMap.Strict#toList` returns a list containing the (key,
value) pairs in the map ``m``, the order is unspecified.

::

    HashMap.toList (HashMap.fromList [(1,"one"), (2,"two"), (3,"three")])
    > [(1,"one"),(2,"two"),(3,"three")]

    HashMap.toList (HashMap.fromList [(1,"one"), (2,"two"), (-3,"negative three")])
    > [(1,"one"),(2,"two"),(-3,"negative three")]


Querying
^^^^^^^^

Lookup an entry in the map (lookup)
"""""""""""""""""""""""""""""""""""

::

    HashMap.lookup :: k -> HashMap k v -> Maybe v
    HashMap.lookup key m = ...

:haddock_short:`/Data.HashMap.Strict#lookup` the value corresponding to the given
``key``, returns ``Nothing`` if the key is not present.

If you want to provide a default value if the key doesn't exist you can use:

::

    HashMap.lookupDefault :: v -> k -> HashMap k v -> v
    HashMap.lookupDefault defaultVal key m = ...

For example::

    import Data.HashMap.Strict ((!?))

    HashMap.lookup 1 HashMap.empty
    > Nothing

    HashMap.lookup 1 (HashMap.fromList [(1,"one"),(2,"two"),(3,"three")])
    > Just "one"

    > (HashMap.fromList [(1,"one"),(2,"two"),(3,"three")]) !? 1
    > Just "one"

    HashMap.lookupDefault "?" k HashMap.empty
    > "?"

    HashMap.lookupDefault "?" 1 (Map.fromList [(1,"one"), (2,"two"), (3,"three")])
    > "one"

.. WARNING::
   **DO NOT** Use ``HashMap.!``. It is partial and throws a runtime error if
   the key doesn't exist.

Check if a map is empty
"""""""""""""""""""""""

::

    HashMap.null :: HashMap k v -> Bool
    HashMap.null m = ...

:haddock_short:`/Data.HashMap.Strict#null` returns ``True`` if the map ``m`` is
empty and ``False`` otherwise.

::

    HashMap.null HashMap.empty
    > True

    HashMap.null (HashMap.fromList [(1,"one")])
    > False

The number of entries in a map
""""""""""""""""""""""""""""""

::

    HashMap.size :: HashMap k v -> Int
    HashMap.size m = ...

:haddock_short:`/Data.HashMap.Strict#size` returns the number of entries in the map
``m``.

::

    HashMap.size HashMap.empty
    > 0

    HashMap.size (HashMap.fromList [(1,"one"), (2,"two"), (3,"three")])
    > 3


Modification
^^^^^^^^^^^^

Adding a new entry to a map
"""""""""""""""""""""""""""

::

    HashMap.insert :: k -> v -> HashMap k v -> HashMap k v
    HashMap.insert key value m = ...

:haddock_short:`/Data.HashMap.Strict#insert` adds the ``value`` into the map
``m`` with the given ``key``, replacing the existing value if the key already
exists.

::

    HashMap.insert 1 "one" HashMap.empty
    > HashMap.fromList [(1,"one")]

    HashMap.insert 4 "four" (HashMap.fromList [(1,"one"), (2,"two"), (3,"three")])
    > fromList [(1,"one"),(2,"two"),(3,"three"),(4,"four")]

    HashMap.insert 1 "uno" (HashMap.fromList [(1,"one"), (2,"two"), (3,"three")])
    > fromList [(1,"uno"),(2,"two"),(3,"three")]


Removing an entry from a map
""""""""""""""""""""""""""""

::

    HashMap.delete :: k -> HashMap k v -> HashMap k v
    HashMap.delete key m = ...

:haddock_short:`/Data.HashMap.Strict#delete` removes the entry with the
specified ``key`` from the map ``m``.  If the key doesn't exist it leaves the
map unchanged.

::

    HashMap.delete 1 HashMap.empty
    > HashMap.empty

    HashMap.delete 1 (HashMap.fromList [(1,"one"),(2,"two"),(3,"three")])
    > fromList [(2,"two"),(3,"three")]

Filtering map entries
"""""""""""""""""""""

::

    HashMap.filterWithKey :: (k -> v -> Bool) -> HashMap k v -> HashMap k v
    HashMap.filterWithKey predicate m = ...

:haddock_short:`/Data.HashMap.Strict#filterWithKey` produces a map consisting of
all entries of ``m`` for which the ``predicate`` returns ``True``.

::

    let f key value = key == 2 || value == "one"
    HashMap.filterWithKey f (HashMap.fromList [(1,"one"), (2,"two"), (3,"three")])
    > fromList [(1,"one"),(2,"two"]


Modifying a map entry
"""""""""""""""""""""

::

    HashMap.adjust :: (v -> v) -> k -> HashMap k v -> HashMap k v
    HashMap.adjust f key m = ...

:haddock_short:`/Data.HashMap.Strict#adjust` applies the value transformation
function ``f`` to the entry with given ``key``. If no entry for that key exists
then the map is left unchanged.

::

    HashMap.alter :: (Maybe v -> Maybe v) -> k -> HashMap k v -> HashMap k v
    HashMap.alter f key m = ...

Apply the value transformation function ``f`` to the entry with given ``key``,
if no entry for that key exists then the function is passed ``Nothing``. If the
function returns ``Nothing`` then the entry is deleted, if the function returns
``Just v2`` then the value for the ``key`` is updated to ``v2``. In other words,
alter can be used to insert, update, or delete a value.

::

    import Data.Maybe (isJust)
    let addValueIfMissing mv = if isJust mv then mv else (Just 1)
    HashMap.alter addValueIfMissing "key" (HashMap.fromList [("key", 0)])
    > fromList [("key",0)]

    let addValueIfMissing mv = if isJust mv then mv else (Just 1)
    HashMap.alter addValueIfMissing "new_key" (HashMap.fromList [("key", 0)])
    > fromList [("key",0),("new_key",1)]

The function ``doubleIfPositive`` below will need to be placed in a Haskell
source file.

::

    doubleIfPositive :: Maybe Int -> Maybe Int
    doubleIfPositive mv = case mv of
      -- Do nothing if the key doesn't exist.
      Nothing -> Nothing

      -- If the key does exist, double the value if it is positive.
      Just v -> if v > 0 then (Just v*2) else (Just v)

    -- In GHCi
    HashMap.alter doubleIfPositive "a" (HashMap.fromList [("a", 1), ("b", -1)])
    > HashMap.fromList [("a",2), ("b",-1)]

    HashMap.alter doubleIfPositive "b" (HashMap.fromList [("a", 1), ("b", -1)])
    > HashMap.fromList [("a", 1), ("b",-1)]

Modifying all map entries (mapping and traversing)
""""""""""""""""""""""""""""""""""""""""""""""""""

::

    HashMap.map :: (a -> b) -> HashMap k a -> HashMap k v
    HashMap.map f m = ...

    HashMap.mapWithKey :: (k -> a -> b) -> HashMap k a -> hashMap k b
    HashMap.mapWithKey g m = ...


:haddock_short:`/Data.HashMap.Strict#map` creates a new map by applying the
transformation function ``f`` to each entries value. This is how `Functor
<https://wiki.haskell.org/Typeclassopedia#Functor>`_ is defined for maps.

:haddock_short:`/Data.HashMap.Strict#mapWithKey` does the same as ``map`` but
gives you access to the key in the transformation function ``g``.

::

    HashMap.map (*10) (HashMap.fromList [("haskell", 45), ("idris", 15)])
    > fromList [("haskell",450),("idris",150)]

    -- Use the Functor instance for Map.
    (*10) <$> HashMap.fromList [("haskell", 45), ("idris", 15)]
    > fromList [("haskell",450),("idris",150)]

    let g key value = if key == "haskell" then (value * 1000) else value
    HashMap.mapWithKey g (HashMap.fromList [("haskell", 45), ("idris", 15)])
    > fromList [("haskell",45000),("idris",15)]


You can also apply a function which performs *actions* (such as printing) to
each entry in the map.

::

    HashMap.traverseWithKey :: Applicative t => (k -> a -> t b) -> HashMap k a -> t (HashMap k b)
    HashMap.traverseWithKey f m = ...

:haddock_short:`/Data.HashMap.Strict#traverseWithKey` maps each element of the
map ``m`` to an *action* that produces a result of type ``b``. The actions are
performed and the values of the map are replaced with the results from the
function. You can think of this as a ``map`` with affects.

::

    -- | Ask the user how they want to schedule a bunch of tasks
    -- that the boss has assigned certain priorities.
    makeSchedule :: HashMap Task Priority -> IO (HashMap Task DateTime)
    makeSchedule = traverseWithKey $ \task priority ->
      do
        putStrLn $ "The boss thinks " ++ show task ++
	             " has priority " ++ show priority ++
                     ". When do you want to do it?"
        readLn



Set-like Operations
^^^^^^^^^^^^^^^^^^^

.. _union:

Union
"""""

::

    HashMap.unionWith :: (v -> v -> v) -> HashMap k v -> HashMap k v -> HashMap k v
    HashMap.unionWith f l r = ...

:haddock_short:`/Data.HashMap.Strict#union` returns a map containing all entries that
are keyed in either of the two maps. If the same key appears in both maps, the
value is determined by calling ``f`` passing in the left and right value (`set
union <https://en.wikipedia.org/wiki/Union_(set_theory)>`_).

::


    HashMap.unionWith (++) HashMap.empty (HashMap.fromList [(1,"x"),(2,"y")])
    > fromList [(1,"x"),(2,"y")]

    let f lv rv = lv
    HashMap.unionWith f (HashMap.fromList [(1, "a")]) (HashMap.fromList [(1,"x"),(2,"y")])
    > fromList [(1,"a"),(2,"y")]

    HashMap.unionWith (++) (HashMap.fromList [(1, "a")]) (HashMap.fromList [(1,"x"),(2,"y")])
    > fromList [(1,"ax"),(2,"y")]


Intersection
""""""""""""

::

    HashMap.intersectionWith :: (v -> v -> v) -> HashMap k v -> HashMap k v -> HashMap k v
    HashMap.intersectionWith f l r = ...

:haddock_short:`/Data.HashMap.Strict#intersection` returns a map containing all
entries that have a key in both maps ``l`` and ``r``. The value in the returned
map is determined by calling ``f`` on the values from the left and right map
(`set intersection <https://en.wikipedia.org/wiki/Intersection_(set_theory)>`_).

::

    HashMap.intersectionWith (++) HashMap.empty (HashMap.fromList [(1,"x"), (2,"y")])
    > fromList []

    HashMap.intersectionWith (++) (HashMap.fromList [(1, "a")]) (HashMap.fromList [(1,"x"),(2,"y")])
    > fromList [(1,"ax")]



Difference
""""""""""

::

    HashMap.difference :: HashMap k v -> HashMap k v -> HashMap k v
    HashMap.difference l r = ...

:haddock_short:`/Data.HashMap.Strict#difference` returns a map containing all
entries that have a key in the ``l`` map but not the ``r`` map (`set
difference/relative complement
<https://en.wikipedia.org/wiki/Complement_(set_theory)#Relative_complement>`_).

::

    HashMap.difference (HashMap.fromList [(1,"one"), (2,"two"), (3,"three")]) HashMap.empty
    > fromList [(1,"uno"),(2,"two"),(3,"three")]

    HashMap.difference (HashMap.fromList[(1,"one"), (2,"two")]) (HashMap.fromList [(1,"uno")])
    > fromList [(2,"two")]


Serialization
-------------

TODO(m-renaud): Serialization docs.

Performance
-----------

The API docs are annotated with the Big-*O* complexities of each of the map
operations. For benchmarks see the `haskell-perf/dictionaries
<https://github.com/haskell-perf/dictionaries>`_ page.


Looking for more?
-----------------

Didn't find what you're looking for? This tutorial only covered the most common
map functions, for a full list of functions see the
:haddock_short:`/Data.HashMap.Strict#HashMap` API documentation.
