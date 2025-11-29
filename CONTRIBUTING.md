# Contributing

## Building, testing, benchmarking

Building the library:

```
cabal build
```

Running the testsuite:

```
cabal test
```

Viewing the test options:

```
cabal run tests -- --help
```

Running a specific property test with an increased number of test cases
(default: 100 cases):

```
cabal run tests -- -p '/All.Properties.Data.HashSet.basic interface.member/' --quickcheck-tests 100_000
```

Running the benchmarks:

```
cabal bench
```

Viewing the benchmark options:

```
cabal run benches -- --help
```

Running a specific benchmark with a reduced target standard deviation (default:
5%):

```
cabal run benches -- -p /All.HashMap.lookup-miss.ByteString/ --stdev 1
```

To include comparison benchmarks for `containers` and `hashmap` uncomment the
`cpp-options` in the benchmark section of `unordered-containers.cabal`:

```
cpp-options: -DBENCH_containers_Map -DBENCH_containers_IntMap -DBENCH_hashmap_Map
```

### References

* [Documentation for `cabal`](https://cabal.readthedocs.io/en/latest/)
* [Documentation for our testing framework, `tasty`](https://github.com/UnkindPartition/tasty#readme)
* [Documentation for our benchmark framework, `tasty-bench`](https://github.com/Bodigrim/tasty-bench#readme)


## Inspecting the generated code

The file `cabal.project.local.dump-code` contains `ghc-options` for dumping Core and
other forms of generated code. To generate these dump files, run

```
cp cabal.project.local.dump-code cabal.project.local
cabal clean
cabal build benches tests
```

You can find the resulting `.dump-*` files in `dist-newstyle/build/*/*/unordered-containers-*/`, e.g.

```
$ tree dist-newstyle/build/*/*/unordered-containers-*/
dist-newstyle/build/x86_64-linux/ghc-9.12.2/unordered-containers-0.2.20.1/
├── b
│   ├── fine-grained
│   │   ├── build
│   │   │   └── fine-grained
│   │   │       ├── autogen
│   │   │       │   ├── cabal_macros.h
│   │   │       │   ├── PackageInfo_unordered_containers.hs
│   │   │       │   └── Paths_unordered_containers.hs
│   │   │       ├── fine-grained
│   │   │       └── fine-grained-tmp
│   │   │           ├── benchmarks
│   │   │           │   ├── FineGrained.dump-asm
│   │   │           │   ├── FineGrained.dump-cmm
│   │   │           │   ├── FineGrained.dump-prep
│   │   │           │   ├── FineGrained.dump-simpl
...
```

To visually compare the generated code from two different states of the source tree,
you can copy the `dist-newstyle/build/*/*/unordered-containers-*/` directory from
each state to separate directories and then use a diff tool like
[Meld](https://meldmerge.org/) to compare them:

```
cabal clean
cabal build benches tests
mkdir dump-$(git rev-parse HEAD)
cp -r dist-newstyle/build/*/*/unordered-containers-*/ dump-$(git rev-parse HEAD)
# Repeat at different commit
meld dump-*
```

Because the files with the generated code are so large, it can be helpful to
temporarily add a small "playground module" to the library:

In `unordered-containers`:

```diff
library:
  exposed-modules:
     ...
     Data.HashSet
     Data.HashSet.Internal
+    Playground
```

`Playground.hs`:

```
{-# language TypeApplications #-}
module Playground where

import qualified Data.HashMap.Internal as HMI
import qualified Data.HashMap.Strict as HS

f = HS.union @String
```

### References

* [A collection of resources on GHC Core](https://stackoverflow.com/q/6121146/1013393)
* [Some links about STG](https://stackoverflow.com/a/12118567/1013393)
* [GHC User's Guide: _Debugging the compiler_](http://downloads.haskell.org/~ghc/latest/docs/html/users_guide/debugging.html)


## Code style

This package uses [`stylish-haskell`](https://hackage.haskell.org/package/stylish-haskell)
to format language pragmas and import sections. To format a specific file, run

```
stylish-haskell -i FILENAME
```

To format all the Haskell files under a specific directory, run

```
stylish-haskell -ir DIRNAME
```
