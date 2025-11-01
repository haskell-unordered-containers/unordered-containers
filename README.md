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

TODO (few collisions, good HAMT shape)

#### Make sure that your keys' `hash` method is fast

TODO

#### Make sure that your keys' `(==)` method is fast

TODO (pointer equality!)

### More documentation

For background information and design considerations on this package see the
[Developer Guide](docs/developer-guide.md).

For practical advice for contributors see [`CONTRIBUTING.md`](CONTRIBUTING.md).
