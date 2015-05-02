# 0.8
* Exported `formatRealFloatB` and `formatRealFloatAltB` from `Text.Show.Text.Data.Floating`. Reexported `FPFormat` (from `text`) in the same module, and added a `Text` `Show` instance for it.
* Added `showbSingPrec` to `Text.Show.Text.GHC.TypeLits` (if using `base-4.6`)
* Modules which were previously exported only if using a recent-enough version of GHC/`base` (e.g., `Text.Show.Text.GHC.Generics`) are now always exposed. If the functionality that the module provides is not available on a given version of GHC/`base`, the module will not expose anything.
* Bump lower version bounds of `text` to 0.11.1 due to reexporting `FPFormat`
* Added `showbUnicodeException`, `showbI16Prec`, `showbDecodingPrec`, and `showbSizePrec` functions (and corresponding `Show` instances) to `Text.Show.Text.Data.Text`
* Made `GShow` in `Text.Show.Text.Generics` poly-kinded
* The Template Haskell deriver (and `GShow`) now handles "infix" data constructors that are applied as prefix correctly (e.g., `data Amp a = (:&) a a`)
* The Template Haskell deriver now handles showable unlifted types (`Char#`, `Double#`, `Float#`, `Int#`, and `Word#`) correctly on GHC 7.11 and later
* The Template Haskell derive now does not parenthesize record types regardless of precedence on GHC 7.11 and later
* Fixed build on GHC 7.2
* Changes test-suite to use `hspec`, which allows for it to be built on GHC 7.0 and 7.2

* TODO: `Show1` and `Show2` classes a la `transformers`. Derivable?
* TODO: Have `base-compat` backport `showFFloatAlt` and `showGFloatAlt`, and use them in the test suite
* TODO: Re-add `Proxy Int` generic show test once `tagged` gets fixed

### 0.7.0.1
* Disabled `print`-related tests, as they sporadically break referential transparency for unknown reasons
* Fixed build on Windows

# 0.7
* Added `showbConstPrec` (and corresponding `Show` and `Show1` instances for `Const`) to `Text.Show.Text.Control.Applicative`
* Added `showbUArrayPrec` (and corresponding `Show` instance for `UArray`s) and `showbIArrayPrec` to `Text.Data.Text.Data.Array`.
* Renamed `showbListDefault` to `showbListWith` to match how `Text.Show` names it
* Exposed `showbShortByteString` with all versions of `bytestring` by using the `bytestring-builder` package
* Corrected the `Show` instance for `Lexeme` (in `Text.Show.Text.Text.Read.Lex`)
* Fixed `TypeRep` output on GHC 7.10 and later
* Removed `LitChar` and `LitString` from `Text.Show.Text.Data.Char`, as they were not as useful as I had imagined.
* Removed the deprecated `replicateB` function
* `Typable` instances for `Show`, `Show1`, and `GShow` (with GHC 7.8 and later)
* `Typeable` instance for `ConType`
* Only derive `Eq` and `Ord` for `ConType` if a recent-enough version of `text` is used
* Changed the implementations of some functions in `Text.Show.Text.Debug.Trace` to use `ByteString`s instead of `String`s

### 0.6.0.1
* Forgot to include some header files in `text-show.cabal`

# 0.6
* `deriveShow` can now construct instances for data families, using either the data family name or a data instance constructor as an argument. See the documentation in `Text.Show.Text.TH` for more details.
* Fixed a bug in which infix backticked data constructors (e.g., ```data Add = Int `Plus` Int```) would not be shown correctly.
* Fixed typo in `Text.Show.Text.GHC.RTS.Flags`
* Removed the phantom-type detecting mechanism with `template-haskell-2.9.0.0` or higher. This method of finding phantom types is intrinsically flawed and is not usable on older GHCs. I plan on introducing a more robust mechanism for detecting phantom types on more versions of Template Haskell in `text-show-0.7`.
* Added generics support with the `Text.Show.Text.Generic` and `Text.Show.Text.Debug.Trace.Generic` modules
* Deprecated `replicateB` in favor of `timesN` from the `semigroups` library
* Added `FromTextShow` to `Text.Show.Text`, which admits a `String` `Show` instance for any data type with a `Text` `Show` instance (the counterpart of `FromStringShow`)
* Added `Monoid` and `Semigroup` instances for `FromStringShow`, `Semigroup` instance for `LitString`, `IsChar` instance for `LitChar`, and `IsString` instance for `[LitChar]`
* Changed the `String` `Show` instances of `FromStringShow`, `LitChar`, and `LitString` to more closely match the `Text` `Show` instances. As a result, the `Read` instances for these data types were also changed so that `read . show = read . show = id`.
* Removed the `recent-text` flag. We'll allow users to build with older versions of `text`, but the latest version is recommended. Because of this, the `integer-simple` and `integer-gmp` flags are not needed.
* Removed the `integer-gmp2` flag, as it supported a configuration that didn't actually compile on GHC
* Removed the `transformers-four` flag, as it is not needed now that `transformers-compat` is a dependency

# 0.5
* Fix build for GHC 7.10, old GHC versions, and Windows
* Removed the `Text.Show.Text.Data.Containers` and `Text.Show.Text.Data.Time` modules. The modules for the data types in `containers` and `time` were migrated to a separate library, `text-show-instances`.
* Removed the `-ftext-format` flag, as `text-show` no longer uses `text-format`.
* A [serious bug](https://github.com/bos/text/issues/99) in the `text` package that caused segfaults when building large `Integer`s was fixed in `text-1.2.0.2`. A flag (`-frecent-text`) was added that allows you to take advantage of this.
* Fixed a bug that would cause the output of functions in the `Text.Show.Text.Data.Floating` module to not match `base` depending on what version of `base` is used.
* The type signatures of lambda expressions generated by `mkShow` and related functions were loosened to allow them to be used to "manually" derive `Show` instances for data types with higher-kinded type parameters or type arguments with restricted `Show` instances. This should not be a breaking change; you can simply do more with `mkShow` et al. than you could before. For more information, see the documentation in `Text.Show.Text.TH`.
* Loosened the `Show` instance of `Complex a` to only depend on `Show a` (previously required `RealFloat a`) if using base-4.4.0.0 or later
* Moved `showbRatioPrec` to `Text.Show.Text.Data.Ratio`, `showbComplexPrec` to `Text.Show.Text.Data.Complex`, `showbProxy` to `Text.Show.Text.Data.Proxy`, and `showbFingerprint` to `Text.Show.Text.GHC.Fingerprint`
* Added `deriveShowPragmas` to `Text.Show.Text.TH` to allow users to specify `INLINE` or `SPECIALIZE instance` pragmas with `Show` instances.
* Added `FromStringShow`, `showbSpace`, `showbUnary`, `showbUnary1`, and `showbBinary1` to `Text.Show.Text`
* Added `mkShowList`, `mkShowListLazy`, and `mkShowbList` to `Text.Data.Text.TH`
* For base-4.8.0.0 and above, added the `Text.Show.Text.Data.Functor.Identity`, `Text.Show.Text.Data.Void`, `Text.Show.Text.GHC.RTS.Flags`, `Text.Show.Text.GHC.StaticPtr`, and `Text.Show.Text.Numeric.Natural` modules. Also added `Show` instances for `AllocationLimitExceeded` in `Text.Show.Text.Control.Exception` and `Alt` in `Text.Show.Text.Data.Monoid`. Also fixed the `Show` instance for `Fixed` values.
* Added the `Text.Show.Text.Data.GHC.Conc.Windows` module (Windows-only)
* Added the `Text.Show.Text.Data.OldTypeable` module for base-4.7
* Added the `Text.Show.Text.GHC.TypeLits` module for base-4.6 and above
* Added the `Text.Show.Text.Debug.Trace` and `Text.Show.Text.Debug.Trace.TH` modules as an analog to `Debug.Trace`
* Added the `Show1` class and corresponding instances for unary type constructors.
* Added `LitChar` and `LitString` to `Text.Show.Text.Data.Char`
* Exported `asciiTabB` in `Text.Show.Text.Data.Char`
* Renamed `showbTextStrict` to 'showbText' (to keep with naming conventions in the `text` library) and added `showbBuilder` to `Text.Show.Text.Data.Text`.

## 0.4.1
* Added the utility functions `toText` and `toString` for working with `Builder`s.

# 0.4
* Due to [GHC bug #5289](http://ghc.haskell.org/trac/ghc/ticket/5289), projects that depend on the `double-conversion` library (such as `text-format`, a dependency of `text-show`) may break due to GHC incorrectly linking against libstdc++. Therefore, `text-show` was changed so that it does not depend on `text-format` by default. This behavior can be changed by using the `-ftext-format` flag when using `cabal`.
* Added `showbZonedTime` to `Text.Show.Text.Data.Time` (and corresponding `Show` instance for `ZonedTime`)
* Exposed `showbMaskingState` (is was already there, I just forgot to export it)
* If using GHC 7.6 or earlier, depend on tagged so that `Data.Proxy` (and thus `showbProxy` from `Text.Show.Text.Data.Typeable`) can be used
* Refactored code to use Template Haskell derivations when possible

## 0.3.1.0
* Added `showList` and `showListLazy`
* Don't use `showbListDefault` to show `containers` data types
* Added the ability to splice `show` functions for arbitrary data types (even if they aren't `Show` instances). These functions are `mkShow`, `mkShowLazy`, `mkShowPrec`, `mkShowPrecLazy`, `mkShowb`, `mkShowbPrec`, `mkPrint`, `mkPrintLazy`, `mkHPrint`, and `mkHPrintLazy`.

# 0.3.0.0
* Lots of bugfixes
* `Show` instances for many other data types in `base`, `containers` and `time`
* Exposed internal modules with monomorphic functions
* `Text.Show.Text` now exports `Data.Text.Lazy.Builder` for convenience
* Add `showLazy`, `showPrec`, `showPrecLazy`, `printLazy`, `hPrint`, `hPrintLazy`, `lengthB`, and `replicateB`
* Template Haskell derivation of `Show` instances (doesn't support data families yet)

# 0.2.0.0
* Added `Show` instances for strict and lazy `Text`

# 0.1.0.0
* Initial commit
