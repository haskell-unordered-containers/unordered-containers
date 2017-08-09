## next

 * Add `Ord/Ord1/Ord2` instances.
 * Remove support for GHC versions before 7.8. (Thanks, Dmitry Ivanov!)
 * Use `SmallArray#` instead of `Array#` for GHC versions 7.10 and above.
   (Thanks, Dmitry Ivanov!)

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
