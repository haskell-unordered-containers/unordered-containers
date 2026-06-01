## unordered-containers

Efficient hashing-based container types.  The containers have been optimized for
performance critical use, both in terms of large data quantities and high speed.

The declared cost of each operation is either worst-case or amortized, but
remains valid even if structures are shared.

### How to get the best performance out of this package

#### Enable hardware support for `popCount`

This package makes heavy use of the `popCount` function. To allow GHC to use the
`popcnt` instruction available on modern x86 and x86-64 platforms, enable GHC's
`-msse4.2` option. Since code from `unordered-containers` may spread to other
packages due to inlining and specialization, it's best to use this setting for
all packages in your project:

In your `cabal.project`:

```
package *
  ghc-options: -msse4.2
```

In your `stack.yaml`:

```
ghc-options:
  "$everything": -msse4.2
```

#### Make sure that your keys have a good hash distribution

`HashMap` and `HashSet` store their contents in a tree that branches on
successive groups of bits taken from the keys' hash values. They perform best
when those hashes are well distributed:

* Keys with *identical* hashes cannot be separated by the tree at all. They end
  up together in a collision node, where lookups and updates degrade to a linear
  scan. A hash function that produces frequent collisions can therefore turn
  near-`O(1)` operations into `O(n)` ones.
* Keys whose hashes merely *share many bits* produce deeper, more sparsely
  populated trees, which means more indirections per operation and higher memory
  use.

Note that all bits of the hash matter, since different levels of the tree use
different ranges of bits.

#### Make sure that your keys' `hash` method is fast

`hash` is evaluated for essentially every `lookup`, `insert`, `delete`, etc., so
it sits squarely on the hot path. For composite keys (records, tuples, nested
structures) hashing can easily dominate the cost of an operation.

If you repeatedly use the same expensive-to-hash keys, consider wrapping them in
[`Data.Hashable.Hashed`](https://hackage.haskell.org/package/hashable/docs/Data-Hashable.html#t:Hashed),
which caches the hash so that it is computed only once per key.

#### Make sure that your keys' `(==)` method is fast

Once the tree has narrowed the candidates down using hash bits, `(==)` is used
to confirm that a stored key really matches the query key (and to pick the right
entry within a collision node). For large or deeply nested keys this comparison
can be costly.

A useful trick for keys that are often physically shared (for example interned
strings, or values drawn from a shared pool) is to start `(==)` with a
pointer-equality check, so that the common "same object" case returns `True`
immediately without inspecting the contents. The `Eq` instances of `Text` and
`ByteString` already short-circuit on length, and `Hashed` compares cached
hashes before comparing the underlying values.

### More documentation

For background information and design considerations on this package see the
[Developer Guide](docs/developer-guide.md).

For practical advice for contributors see [`CONTRIBUTING.md`](CONTRIBUTING.md).
