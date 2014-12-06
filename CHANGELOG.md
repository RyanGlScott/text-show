# 0.3.1.0
* Add `showList` and `showListLazy`
* Don't use showbListDefault to show `containers` data types
* Add the ability to splice `show` functions for arbitrary data types (even if they aren't `Show` instances). These functions are `mkShow`, `mkShowLazy`, `mkShowPrec`, `mkShowPrecLazy`, `mkShowb`, `mkShowbPrec`, `mkPrint`, `mkPrintLazy`, `mkHPrint`, and `mkHPrintLazy`.

# 0.3.0.0
* Lots of bugfixes
* Show instances for many other data types in `base`, `containers` and `time`
* Expose internal modules with monomorphic functions
* `Text.Show.Text` now exports `Data.Text.Lazy.Builder` for convenience
* Add `showLazy`, `showPrec`, `showPrecLazy`, `printLazy`, `hPrint`, `hPrintLazy`, `lengthB`, and `replicateB`
* Template Haskell derivation of `Show` instances (doesn't support data families yet)

# 0.2.0.0

* Add `Show` instances for strict and lazy `Text`

# 0.1.0.0

* Initial commit
