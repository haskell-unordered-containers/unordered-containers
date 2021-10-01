The salt changes are backwards compatible.

However, if you want to let a client make use of
custom salts some effort is required,
running commands like these should get you somewhere:

```ed
:%s/HashMap k/HashMapT salt k/g
:%s/Hashable k)/Hashable k, KnownNat salt)
```

HashMap is now an alias to HashMapT with a hardcoded DefaultSalt.
These changes allow salt to be anything.
semigroup operations (a -> a -> a) can use the same salt to guarantee
not having to rebuild the hahsmap.


If you encounter this error:
```
    • Illegal instance declaration for ‘SomeTypeClass (HashMap k v)’
        (All instance types must be of the form (T t1 ... tn)
         where T is not a synonym.
         Use TypeSynonymInstances if you want to disable this.)
```
usually it's good enough to provide the instance with a free salt:

```haskell
instance SomeTypeClass (HashMap salt k v) where
  ...

```
If it it starts complaining about not salt not matching DefaultSalt,
use the `'` constructors such as `empty'` and `singleton'`
