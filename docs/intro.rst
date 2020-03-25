``unordered-containers`` Introduction
=====================================

The ``unordered-containers`` package provides implementations of various
hash-based immutable data structures.

Some of the data structures provided by this package have a very large API
surface (for better or worse). The docs here focus on the most common functions
which should be more than enough to get you started. Once you know the basics,
or if you're looking for a specific function, you can head over to the
:haddock:`unordered-containers` Haddocks to check out the full API
documentation!

Provided Data Structures
------------------------

- :doc:`hash-set`: unordered, non-duplicated elements
- :doc:`hash-map`: unordered maps from keys to values (aka. dictionaries)


Related Packages
----------------

- :haddock:`containers` - ordered containers using trees instead of
  hashing.

- :haddock:`hashable` - types that can be converted to a hash value.

- :haddock:`hashtables` - mutable hash tables in the ST monad.


Looking for more resources?
---------------------------

If you've worked your way through the documentation here and you're looking for
more examples or tutorials you should check out:

- `haskell-lang.org's containers tutorial
  <https://haskell-lang.org/library/containers>`_, its focused on the ordered
  ``containers`` library but provides some useful examples.
- `Learn You a Haskell "Modules" chapter <http://learnyouahaskell.com/modules>`_

.. _installing:

Installing and using the ``unordered-containers`` package
---------------------------------------------------------

Version Requirements
^^^^^^^^^^^^^^^^^^^^

All of the examples here should work for all recent versions of the package.


Importing modules
^^^^^^^^^^^^^^^^^

All of the modules in ``unordered-containers`` should be imported ``qualified``
since they use names that conflict with the standard Prelude.

::

    import qualified Data.HashSet as HashSet
    import qualified Data.HashMap.Strict as HashMap


In GHCi
^^^^^^^

Start the GHCi `REPL
<https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop>`_ with
``ghci``, ``cabal repl``, or ``stack ghci``. Once the REPL is loaded, import the
modules you want using the ``import`` statements above and you're good to go!


In a `Cabal <https://cabal.readthedocs.io>`_ or `Stack <https://www.haskellstack.org>`_ project
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Add ``unordered-containers`` to the ``build-depends:`` stanza for your library,
executable, or test-suite::

    library
        build-depends:
	    base >= 4.3 && < 5,
	    unordered-containers >= 0.2.7 && < 0.3

and ``import`` any modules you need in your Haskell source files.
