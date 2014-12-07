# 0.4
* Due to [GHC bug #5289](http://ghc.haskell.org/trac/ghc/ticket/5289), projects that depend on the `double-conversion` library (such as `text-format`, a dependency of `text-show`) may break due to GHC incorrectly linking against libstdc++. Therefore, `text-show` was changed so that it does not depend on `text-format` by default. This behavior can be changed by using the `-ftext-format` flag when using `cabal`.
* Add `showbZonedTime` to `Text.Show.Text.Data.Time` (and corresponding `Show` instance for `ZonedTime`)
* Expose `showbMaskingState` (is was already there, I just forgot to export it)
* If using GHC 7.6 or earlier, depend on tagged so that Data.Proxy can be used
* Refactor code to use Template Haskell derivations when possible

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
