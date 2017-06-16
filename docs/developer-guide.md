# Developer Guide

This guide is meant as an entry point for developer. It both gives the
philisophy behind the design of this package and some concrete details, such as
invariants.

## Why does this package exist?

This package exists to offer a different performance/functionality
trade-of vis-a-vis ordered container packages
(e.g. [containers](http://hackage.haskell.org/package/containers)). Hashing-based
data structures tend to be faster than comparison-based ones, at the cost of not
providing operations the rely on the data being ordered.

This means that this package must be faster than ordered containers, or there
would be no reason for it to exist, given that its functionality is a strict
subset of ordered containers. This might seem obvious, but the author has
rejected several proposals in the past (e.g. to switch to higher quality but
slower hash functions) that would have made unordered-containers too slow to
motivate its existance.

## A note on hash functions

While the [hashable](http://hackage.haskell.org/package/containers) package is a
separate package, it was co-designed with this package. Its main role is to
support this package and not to provide good general purpose hash functions
(e.g. to use when fingerprinting a text file).

The hash functions used (by default) were picked to make data structures
fast. The actual functions used oftens surprise developers who have learned
about hashing during their studies but haven't looked at which functions are
actually used in practice.

For example, integers are hashed to themselves. This might seemed contrary to
what you might have learned about hashing (e.g. that you need avalanche
behavior; changing one bit of input changes half of the bits in the output). It
turns out that this isn't what typically is done in practice (take a little tour
of the various programming languages standard libraries to see this for
yourself). Hashing integers to themselves is both faster (i.e. free) and the
improved locality can be helpful given common input patterns.

Another interesting example of hashing is string hashing, where
[FNV](https://en.wikipedia.org/wiki/Fowler%E2%80%93Noll%E2%80%93Vo_hash_function)
is used. FNV is a decent hash function, but hash worse properties than say
[MurmurHash](https://en.wikipedia.org/wiki/MurmurHash). However, it's much
faster. The fact that it's faster is not obvious given the way hash function
performance is often quoted, namely by giving the average throughput on large
inputs. Most inputs (e.g. keys) aren't large, often no more than 10 characters
long. Hash functions typically have a start-up cost and many functions that have
high throughput (such as MurmurHash) are more expensive for short strings than
FNV.

### Security

There's an uncomfortable trade-off with regards to security threats posed by
e.g. denial of service attacks. Always using more secure hash function, like
[SipHash](https://en.wikipedia.org/wiki/SipHash), would provide security by
default. However, those functions would make the performance of the data
structures no better than that of ordered containers, which defeats the purpose
of this package.

The current, someone frustrating, state is that you have to know which data
structures can be tampered with by users and either use SipHash just for those
or switch to ordered containers that don't have collision problems. This package
uses fast hash functions by default.
