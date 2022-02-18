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

To include comparison benchmarks for `containers` and `hashmap` uncomment the
`cpp-options` in the benchmark section of `unordered-containers.cabal`:

```
cpp-options: -DBENCH_containers_Map -DBENCH_containers_IntMap -DBENCH_hashmap_Map
```

### References

* [Documentation for `cabal`](https://cabal.readthedocs.io/en/latest/)
* [Documentation for our testing framework, `tasty`](https://github.com/UnkindPartition/tasty#readme)
* [Documentation for our benchmark framework, `tasty-bench`](https://github.com/Bodigrim/tasty-bench#readme)
