## [0.2.21] - December 2025

* API enhancements:
  * [Add `HashMap.lookupKey` and `HashSet.lookupElement`](https://github.com/haskell-unordered-containers/unordered-containers/pull/554)
  * [Add `differenceWithKey`](https://github.com/haskell-unordered-containers/unordered-containers/pull/542)
  * [Add `disjoint`](https://github.com/haskell-unordered-containers/unordered-containers/pull/559)

* Performance improvements:
  * [`HashSet.fromList`: Use `unsafeInsert`](https://github.com/haskell-unordered-containers/unordered-containers/pull/515)
  * [Use tree-diffing for `difference`](https://github.com/haskell-unordered-containers/unordered-containers/pull/535)
  * [Remove some unnecessary forcing of HashMaps](https://github.com/haskell-unordered-containers/unordered-containers/pull/545)
  * [Remove the `Array.index` function](https://github.com/haskell-unordered-containers/unordered-containers/pull/539)
  * [`hashWithSalt`: Ensure that the salt `Int` is unboxed](https://github.com/haskell-unordered-containers/unordered-containers/pull/569)

* Documentation changes:
  * [Turn some comments into docstrings](https://github.com/haskell-unordered-containers/unordered-containers/pull/516)
  * [Reword disclaimer regarding hash collision attacks](https://github.com/haskell-unordered-containers/unordered-containers/pull/557)
  * [Update time complexity of some HashSet functions](https://github.com/haskell-unordered-containers/unordered-containers/pull/568)
  * [Update instructions for code inspection](https://github.com/haskell-unordered-containers/unordered-containers/pull/567)

* Other changes:
  * [Drop support for GHC < 8.10](https://github.com/haskell-unordered-containers/unordered-containers/pull/510)
  * [Address deprecation warnings and other warnings](https://github.com/haskell-unordered-containers/unordered-containers/pull/512)
  * [Optimize indexing in arrays of length 2](https://github.com/haskell-unordered-containers/unordered-containers/pull/528)
  * [Introduce `ShiftedHash`](https://github.com/haskell-unordered-containers/unordered-containers/pull/529)
  * [New "fine-grained" benchmarks](https://github.com/haskell-unordered-containers/unordered-containers/pull/526)
  * [Make it compile with MicroHs](https://github.com/haskell-unordered-containers/unordered-containers/pull/553). Thanks, @augustss!
  * [Remove redundant `Eq` constraints](https://github.com/haskell-unordered-containers/unordered-containers/pull/558)
  * [Refactor `delete`](https://github.com/haskell-unordered-containers/unordered-containers/pull/571)
  * [`difference[With]`: Undo constraint relaxation](https://github.com/haskell-unordered-containers/unordered-containers/pull/573)

[0.2.21]: https://github.com/haskell-unordered-containers/unordered-containers/compare/v0.2.20.1...v0.2.21

## [0.2.20.1] - October 2025

* [Fix infinite loop in `isSubmapOf[By]` / `isSubsetOf` on 32-bit platforms](https://github.com/haskell-unordered-containers/unordered-containers/pull/501).
  To fix this bug and potentially other similar bugs, we return to a branching factor of 16 on 32-bit platforms.

* [Relax bounds for GHC 9.12](https://github.com/haskell-unordered-containers/unordered-containers/pull/499)

* [Require `hashable >= 1.4`](https://github.com/haskell-unordered-containers/unordered-containers/pull/506)

* Documentation changes:
  * [Fix documentation about branching factor in `Data.HashMap.Strict`](https://github.com/haskell-unordered-containers/unordered-containers/pull/494)
  * [Improve documentation for `Data.HashMap.compose`](https://github.com/haskell-unordered-containers/unordered-containers/pull/500)
  * [Fixes docs of `Data.HashMap.Lazy.fromList`: it takes O(n * log(n))](https://github.com/haskell-unordered-containers/unordered-containers/pull/498)
  * [Add disclaimer to `Data.HashSet.toList`](https://github.com/haskell-unordered-containers/unordered-containers/pull/507)

* [Remove bad `isSubmapOf` testcase](https://github.com/haskell-unordered-containers/unordered-containers/pull/504)

[0.2.20.1]: https://github.com/haskell-unordered-containers/unordered-containers/compare/v0.2.20...v0.2.20.1

## [0.2.20] - January 2024

* [Allow `template-haskell-2.21`](https://github.com/haskell-unordered-containers/unordered-containers/pull/484)

* [Rename confusing variables](https://github.com/haskell-unordered-containers/unordered-containers/pull/479)

* [Deal with introduction of `Prelude.foldl'`](https://github.com/haskell-unordered-containers/unordered-containers/pull/480)

* [Remove redundant `Hashable` constraints](https://github.com/haskell-unordered-containers/unordered-containers/pull/478)
  from `intersection.*` and `union.*`.

* Various optimizations and cleanups:
  [#458](https://github.com/haskell-unordered-containers/unordered-containers/pull/458),
  [#469](https://github.com/haskell-unordered-containers/unordered-containers/pull/469),
  [#404](https://github.com/haskell-unordered-containers/unordered-containers/pull/404),
  [#460](https://github.com/haskell-unordered-containers/unordered-containers/pull/460),
  [#456](https://github.com/haskell-unordered-containers/unordered-containers/pull/456),
  [#433](https://github.com/haskell-unordered-containers/unordered-containers/pull/433)

* Add invariant tests:
  [#444](https://github.com/haskell-unordered-containers/unordered-containers/pull/444),
  [#455](https://github.com/haskell-unordered-containers/unordered-containers/pull/455)

* [Improve test case generation](https://github.com/haskell-unordered-containers/unordered-containers/pull/442)

* [Improve test failure reporting](https://github.com/haskell-unordered-containers/unordered-containers/pull/440)

[0.2.20]: https://github.com/haskell-unordered-containers/unordered-containers/compare/v0.2.19.1...v0.2.20

## [0.2.19.1] – April 2022

* [Fix bug in `intersection[With[Key]]`](https://github.com/haskell-unordered-containers/unordered-containers/pull/427)

* [Improve docs of bit twiddling functions](https://github.com/haskell-unordered-containers/unordered-containers/pull/396)

[0.2.19.1]: https://github.com/haskell-unordered-containers/unordered-containers/compare/v0.2.19.0...v0.2.19.1

## [0.2.19.0] – April 2022

* [Make intersections much faster](https://github.com/haskell-unordered-containers/unordered-containers/pull/406)

* [Fix undefined behaviour on 32-bit platforms](https://github.com/haskell-unordered-containers/unordered-containers/pull/413)

* Speed up some array-appending operations: [#407](https://github.com/haskell-unordered-containers/unordered-containers/pull/407), [#409](https://github.com/haskell-unordered-containers/unordered-containers/pull/409)

* [Use MathJax format for complexity annotations](https://github.com/haskell-unordered-containers/unordered-containers/pull/411)

[0.2.19.0]: https://github.com/haskell-unordered-containers/unordered-containers/compare/v0.2.18.0...v0.2.19.0

## [0.2.18.0]

* [Fix strictness properties of `Strict.mapMaybe[WithKey]`](https://github.com/haskell-unordered-containers/unordered-containers/pull/385)

* [Fix strictness properties of `Strict.alterFEager`](https://github.com/haskell-unordered-containers/unordered-containers/pull/384)

* [Fix space leaks in `union[With[Key]]`](https://github.com/haskell-unordered-containers/unordered-containers/pull/380)

* [Fix space leak in `Lazy.fromListWith`](https://github.com/haskell-unordered-containers/unordered-containers/pull/386)

* [Speed up `difference*` and `intersection*` with `unsafeInsert`](https://github.com/haskell-unordered-containers/unordered-containers/pull/372)

* [`unionArrayBy`: Find next 1-bits with `countTrailingZeros`](https://github.com/haskell-unordered-containers/unordered-containers/pull/395)
  - This speeds up `union*` for sparsely filled nodes, while penalizing `union` operations on densely filled nodes.

* [Reduce reboxing in internal array operations](https://github.com/haskell-unordered-containers/unordered-containers/pull/377)

* [Reduce code size of array operations in `union*`](https://github.com/haskell-unordered-containers/unordered-containers/pull/376)

[0.2.18.0]: https://github.com/haskell-unordered-containers/unordered-containers/compare/v0.2.17.0...v0.2.18.0

## [0.2.17.0]

* [Define `dataCast1` for `HashMap`](https://github.com/haskell-unordered-containers/unordered-containers/pull/345)

* [Add `Lift` instances for Template Haskell](https://github.com/haskell-unordered-containers/unordered-containers/pull/343)

* [Add definitions for `stimes`](https://github.com/haskell-unordered-containers/unordered-containers/pull/340)

* [Expose internal constructors for `HashSet`, `Array` and `MArray`](https://github.com/haskell-unordered-containers/unordered-containers/pull/347)

* [Tweak internal `Array.insertM` function](https://github.com/haskell-unordered-containers/unordered-containers/pull/359)

* [Drop support for GHC 8.0](https://github.com/haskell-unordered-containers/unordered-containers/pull/354)

* [Drop support for `hashable < 1.2.5`](https://github.com/haskell-unordered-containers/unordered-containers/pull/355)

* Various cleanup and documentation improvements

[0.2.17.0]: https://github.com/haskell-unordered-containers/unordered-containers/compare/v0.2.16.0...v0.2.17.0

## [0.2.16.0]

* [Increase maximum branching factor from 16 to 32](https://github.com/haskell-unordered-containers/unordered-containers/pull/317)

* [Tweak `union.goDifferentHash`](https://github.com/haskell-unordered-containers/unordered-containers/pull/277)

* [Fix debug mode bounds check in `cloneM`](https://github.com/haskell-unordered-containers/unordered-containers/pull/331)

* [Remove some old internal compatibility code](https://github.com/haskell-unordered-containers/unordered-containers/pull/334)

[0.2.16.0]: https://github.com/haskell-unordered-containers/unordered-containers/compare/v0.2.15.0...v0.2.16.0

## [0.2.15.0]

* [Add security advisory regarding hash collision attacks](https://github.com/haskell-unordered-containers/unordered-containers/pull/320)

* [Add support for hashable 1.4](https://github.com/haskell-unordered-containers/unordered-containers/pull/324)

* [Drop support for GHC < 8](https://github.com/haskell-unordered-containers/unordered-containers/pull/323)

[0.2.15.0]: https://github.com/haskell-unordered-containers/unordered-containers/compare/v0.2.14.0...v0.2.15.0

## [0.2.14.0]

* [Add `HashMap.mapKeys`.](https://github.com/haskell-unordered-containers/unordered-containers/pull/308) Thanks, Marco Perone!

* [Add instances for `NFData1` and `NFData2`.](https://github.com/haskell-unordered-containers/unordered-containers/pull/314) Thanks, Isaac Elliott and Oleg Grenrus!

* [Fix `@since`-annotation for `compose`.](https://github.com/haskell-unordered-containers/unordered-containers/pull/303) Thanks, @Mathnerd314!

[0.2.14.0]: https://github.com/haskell-unordered-containers/unordered-containers/compare/v0.2.13.0...v0.2.14.0

## [0.2.13.0]

* [Add `HashMap.compose`.](https://github.com/haskell-unordered-containers/unordered-containers/pull/299) Thanks Alexandre Esteves.

[0.2.13.0]: https://github.com/haskell-unordered-containers/unordered-containers/compare/v0.2.12.0...v0.2.13.0

## [0.2.12.0]

* Add `HashMap.isSubmapOf[By]` and `HashSet.isSubsetOf`. Thanks Sven Keidel. ([#282])

* Expose internal modules. ([#283])

* Documentation improvements in `Data.HashSet`, including a beginner-friendly
  introduction. Thanks Matt Renaud. ([#267])

* `HashMap.alterF`: Skip key deletion for absent keys. ([#288])

* Remove custom `unsafeShift{L,R}` definitions. ([#281])

* Various other documentation improvements.

[0.2.12.0]: https://github.com/haskell-unordered-containers/unordered-containers/compare/v0.2.11.0...v0.2.12.0
[#267]: https://github.com/haskell-unordered-containers/unordered-containers/pull/267
[#281]: https://github.com/haskell-unordered-containers/unordered-containers/pull/281
[#282]: https://github.com/haskell-unordered-containers/unordered-containers/pull/282
[#283]: https://github.com/haskell-unordered-containers/unordered-containers/pull/283
[#288]: https://github.com/haskell-unordered-containers/unordered-containers/pull/288

## 0.2.11.0

 * Add `HashMap.findWithDefault` (soft-deprecates `HashMap.lookupDefault`).
   Thanks, Matt Renaud.

 * Add `HashMap.fromListWithKey`. Thanks, Josef Svenningsson.

 * Add more folding functions and use them in `Foldable` instances. Thanks,
   David Feuer.

 * Add `HashMap.!?`, a flipped version of `lookup`. Thanks, Matt Renaud.

 * Add a `Bifoldable` instance for `HashMap`. Thanks, Joseph Sible.

 * Add a `HasCallStack` constraint to `(!)`. Thanks, Roman Cheplyaka.

### Bug fixes

 * Fix a space leak affecting updates on keys with hash collisions. Thanks,
   Neil Mitchell. ([#254])

 * Get rid of some silly thunks that could be left lying around. ([#232]).
   Thanks, David Feuer.

### Other changes

 * Speed up the `Hashable` instances for `HashMap` and `HashSet`. Thanks,
   Edward Amsden.

 * Remove a dependency cycle hack from the benchmark suite. Thanks,
   Andrew Martin.

 * Improve documentation. Thanks, Tristan McLeay, Li-yao Xia, Gareth Smith,
   Simon Jakobi, Sergey Vinokurov, and likely others.

[#232]: https://github.com/haskell-unordered-containers/unordered-containers/issues/232
[#254]: https://github.com/haskell-unordered-containers/unordered-containers/issues/254

## 0.2.10.0

 * Add `HashMap.alterF`.

 * Add `HashMap.keysSet`.

 * Make `HashMap.Strict.traverseWithKey` force the results before
   installing them in the map.

## 0.2.9.0

 * Add `Ord/Ord1/Ord2` instances. (Thanks, Oleg Grenrus)

 * Use `SmallArray#` instead of `Array#` for GHC versions 7.10 and above.
   (Thanks, Dmitry Ivanov)

 * Adjust for `Semigroup => Monoid` proposal implementation.
   (Thanks, Ryan Scott)

### Bug fixes

 * Fix a strictness bug in `fromListWith`.

 * Enable eager blackholing for pre-8.2 GHC versions to work around
   a runtime system bug. (Thanks, Ben Gamari)

 * Avoid sketchy reimplementation of `ST` when compiling with recent
   GHC.

### Other changes

 * Remove support for GHC versions before 7.8. (Thanks, Dmitry Ivanov)

 * Add internal documentaton. (Thanks, Johan Tibell)

## 0.2.8.0

 * Add `Eq1/2`, `Show1/2`, `Read1` instances with `base-4.9`

 * `Eq (HashSet a)` doesn't require `Hashable a` anymore, only `Eq a`.

 * Add `Hashable1/2` with `hashable-1.2.6.0`

 * Add `differenceWith` function.

## 0.2.7.2

 * Don't use -fregs-graphs

 * Fix benchmark compilation on stack.

## 0.2.7.1

 * Fix linker error related to popcnt.

 * Haddock improvements.

 * Fix benchmark compilation when downloaded from Hackage.

## 0.2.7.0

 * Support criterion 1.1

 * Add unionWithKey for hash maps.

## 0.2.6.0

 * Mark several modules as Trustworthy.

 * Add Hashable instances for HashMap and HashSet.

 * Add mapMaybe, mapMaybeWithKey, update, alter, and
   intersectionWithKey.

 * Add roles.

 * Add Hashable and Semigroup instances.

## 0.2.5.1 (2014-10-11)

 * Support base-4.8
