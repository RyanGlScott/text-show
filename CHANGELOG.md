### next [????.??.??]
* Fix a bug in which `deriveTextShow{1,2}` would needlessly reject data types
  whose last type parameters appear as oversaturated arguments to a type
  family.

### 3.8.5 [2020.02.28]
* Import from `GHC.Exts`, not `GHC.Prim`.

### 3.8.4 [2020.01.28]
* Update the `TextShow(1)` instances for `Down` and `ThreadId` to match their
  implementations in `base-4.14`.

### 3.8.3 [2019.11.06]
* Use `base-compat-batteries-0.11.0` to define instances for `(:~:)` back to
  `base-4.5` and instances for `(:~~:)` back to `base-4.9`.

### 3.8.2 [2019.05.02]
* Make the `TextShow` instances for `UArray` and `Fixed` use the correct
  precedence on `base-4.13` or later.
* Support deriving `TextShow(1)(2)` instances for data types with fields
  of type `Int8#`, `Int16#`, `Word8#`, or `Word16#` on GHC 8.8 or later.

### 3.8.1 [2019.04.26]
* Support `th-abstraction-0.3` or later.

## 3.8 [2019.03.19]
* Remove the `TextShow I16` instance from `TextShow.Data.Text`. The usefulness
  of this instance was dubious at best, and moreover, it was actively causing
  issues when building `text-show` with `reflex-platform` (see
  [#40](https://github.com/RyanGlScott/text-show/issues/40)).

### 3.7.5 [2018.10.07]
* _Actually_ make `showbEFloat`'s behavior match that of `showEFloat` in
  `base-4.12`.
* Remove uses of `AutoDeriveTypeable`, since it is now deprecated. (As a
  result, some things which used to have `Typeable` instances on GHC 7.8
  no longer do, but I'm choosing not to be bothered by this unless someone
  shouts.)

### 3.7.4 [2018.07.03]
* Add `FromGeneric` and `FromGeneric1` newtype adapters to `TextShow.Generic`.
  These are suitable for use with `DerivingVia`, and provide a convenient way
  to obtain `Generic(1)`-based defaults for `TextShow(1)` instances.
* Add `TextShow(1)` instances for `Data.Monoid.Ap` on `base-4.12` or later.
* Make `showbEFloat`'s behavior match that of `showEFloat` in `base-4.12`.

### 3.7.3 [2018.04.07]
* Use `base-compat-batteries`.
* Add a `TextShow FixIOException` instance on `base-4.11` or later.

### 3.7.2 [2018.03.08]
* Depend on `quickcheck-instances-0.3.18` or later.

### 3.7.1 [2017.12.29]
* Don't define a `TextShow GCStats` instance on GHC 8.4 or later, as `GCStats`
  has been removed from `base`. This fixes the build on GHC 8.4.

## 3.7 [2017.12.07]
* Use `EmptyCase` on GHC 7.8 or later for `TextShow(1)` instances for empty data types that are derived using `TextShow.Generic`.
* Derived `TextShow(1)(2)` instances (using `TextShow.TH`) will now force their argument instead of simply `error`ing.
* Add `emptyCaseBehavior` to `Options`, which configures whether derived instances (using `TextShow.TH`) for empty data types should use the `EmptyCase` extension (this is disabled by default).

### 3.6.2 [2017.06.18]
* Drop support for GHC 7.0 and 7.2
* Require `QuickCheck-2.10`/`quickcheck-instances-0.13.6` or later

### 3.6 [2017.04.22]
* Introduce some 'TextShow' instances for datatypes added in `base-4.10.0.0`:
  * `CBool` (from `Foreign.C.Types`)
  * `CBlkSize`, `CBlkCnt`, `CClockId`, `CFsBlkCnt`, `CId`, `CKey`, and `CTimer` (from `System.Posix.Types`)
  * `CompactionFailed` (from `Control.Exception`)
  * `(:~~:)` (from `Data.Type.Equality`)
  * `TypeRep` and `SomeTypeRep` (from `Type.Reflection`)
* Remove most monomorphic `TextShow` functions, as their utility is questionable, and their maintenance burden is undeniable
* Add `showbCommaSpace`, `showtCommaSpace`, and `showtlCommaSpace` to `TextShow`. This appears quite frequently in derived `TextShow` instances, so it is likely worthwhile to define it once.
* Rename `showbVersionConcrete` to `showbVersion` in `TextShow.Data.Version`
* Add `SPECIALIZE` pragmas for `showbEFloat`, `showbFFloat`, and `showbGFloat` in `TextShow.Data.Floating`, mirroring the ones in `Numeric`
* Generalize the kind of the last type parameter for the `TextShow (Const a b)` instance, which previously had been defaulted to `*`
* Refactor test suite

#### 3.4.1.1 [2016.11.07]
* Require `criterion-1.1.4` or later. This allows the benchmarks to be run on older versions of GHC.

### 3.4.1 [2016.10.20]
* Require `QuickCheck-2.9` or later
* Fix bug in which infix record selectors weren't shown with parentheses

## 3.4
* The default definitions of `showt` and `showtl` were changed to `showtPrec 0` and `showtlPrec 0`, respectively
* `deriveTextShowOptions`, `deriveTextShow1Options`, and `deriveTextShow2Options` added to `TextShow.TH`, which allow further configuration of how `TextShow(1)(2)` instances should be derived using the new `Options` data type. `Options` itself contains `GenTextMethods`, which configures whether manual implementations of `TextShow` should implement the methods that return strict and lazy `Text`.
  * The `defaultOptions` uses `SometimesTextMethods`, which only implements the `Text`-returning methods if the datatype contains only nullary constructors (i.e., it is an enumeration type). For example, `deriveTextShow = deriveTextShowOptions defaultOptions`. One can also choose `AlwaysTextMethods` or `NeverTextMethods` instead.
* The internals of `TextShow.Generic` were refactored so that is possible to generically derive `showbPrec`, `showtPrec`, and `showtlPrec` (which use `Builder`, strict `Text`, and lazy `Text`, respectively). Before, only generic derivation of `showbPrec` was possible, and all other generic functions were defined in terms of `showbPrec`.
  * The internal class `GTextShow` was split up into `GShowB`, `GShowT`, and `GShowTL`, depending on what type it returns.
  * As a result, functions like `genericShowtPrec` might be faster than before if they are showing something like an enumeration type, since they no longer construct an intermediate `Builder`. On the other hand, they might be slower if they are showing a constructor with many fields, since they will now be appending lots of `Text`s. If so, make sure to switch to `genericShowbPrec` and convert the final `Builder` to `Text` instead.
* Added `showtParen`, `showtSpace`, `showtlParen`, `showtlSpace`, `liftShowtPrec`, `liftShowtPrec2`, `liftShowtlPrec`, and `liftShowtlPrec2` to `TextShow`
* Added `showtPrecToShowbPrec`, `showtlPrecToShowbPrec`, `showtToShowb`, `showtlToShowb`, `showbPrecToShowtPrec`, `showbPrecToShowtlPrec`, `showbToShowt`, and `showbToShowtl` to `TextShow`
* Added `showtListWith` and `showtlListWith` to `TextShow.Data.List`
* Added `Data` instance for `ConType` in `TextShow.Generic`
* Require `generic-deriving-1.11` or later

## 3.3
* Refactored the internals of `TextShow.Generic` to avoid the use of proxies.
* Made benchmark suite more comprehensive, including benchmarks for showing an enumeration type
* Microoptimization in derived `TextShow1/2` instances involving `TextShow.TH`
* Allow building with `QuickCheck-2.9`
* Fix GHC HEAD build

### 3.2.2
* Added benchmarks

### 3.2.1
* Fixed compilation on GHC 8.0

## 3.2
* Rewrote `TextShow.Generic` to enable more code reuse. The `GTextShow1` and `GTextShow1Con` classes were eliminated, and `GTextShow` and `GTextShowCon` were redesigned to be able to generically implement both `showbPrec` and `liftShowbPrec`. The latter two classes now take an additional `arity` type parameter which is `Zero` if `TextShow` is being derived and `One` is `TextShow1` is being derived.

## 3.1
* Made the derived `TextShow` output of record datatypes match that of `Show` in GHC 8.0 (now that Trac #2530 has been reverted)
* Fixed GHC 8.0 build (again)

### 3.0.1
* Added the `TextShow.GHC.Stack` module, which provides `TextShow` instances for `CallStack` and `SrcLoc` on `base-4.8.1` and up.
* Fix Haddock rendering error

# 3
* GHC 8.0 support
* The functions `showt`, `showtl`, `showtPrec`, `showtlPrec`, `showtList`, and `showtlList` are now part of the `TextShow` class. This was done to allow overriding their definitions with more efficient versions specifically for strict or lazy `Text`. (Currently, no `TextShow` instance in the `text-show` package does this, but this may change in the future.)
* Added the `TextShow.Data.Functor.Compose`, `TextShow.Data.Functor.Product`, `TextShow.Data.Functor.Sum`, and `TextShow.Data.Semigroup` modules
* Added `TextShow` instance for `TypeError` in `TextShow.Control.Exception` (GHC 8.0+)
* Added `TextShow` instances for `TrName` and `Module` in `TextShow.Data.Typeable` (GHC 8.0+)
* Added `Lift` instances for the datatypes in `TextShow` and `TextShow.Generic`
* Renamed the class methods of `TextShow1` and `TextShow2` to be consistent with the naming conventions of `transformers-0.5`. They following were renamed:
  * `showbPrecWith` → `liftShowbPrec`
  * `showbPrecWith2` → `liftShowbPrec2`
  * `makeShowbPrecWith` → `makeLiftShowbPrec`
  * `makeShowbPrecWith2` → `makeLiftShowbPrec2`
  * `genericShowbPrecWith` → `genericLiftShowbPrec`

  In addition, many other monomorphic functions from the various `TextShow` submodules were also renamed to be consistent with the new `lift-` prefix.
* `showsToShowb` and `showbToShows` now only convert functions that ignore precedence (i.e., of type `a -> ShowS` or `a -> Builder`). Their former role has been given to the new functions `showsPrecToShowbPrec` and `showbPrecToShowsPrec`
* Added `FromStringShow1`, `FromTextShow1`, `FromStringShow2`, and `FromTextShow2`, which allow defining string `Show1`/`Show2` instances in terms of `TextShow1`/`TextShow2` instances, and vice versa. Be aware that many of these instances cannot be provided if you are using `tranformers-0.4`, since its version of `Data.Functor.Classes` uses a very differenltly designed `Show1` typeclass (and does not have `Show2` at all).
* Rewrote `TextShow.TH`'s type inferencer. This avoids a nasty GHC 7.8 bug, and it allows `TextShow(1)(2)` to be derived for more datatypes that can only be expressed with `-XTypeInType` enabled.
* Reworked internals of `TextShow.Generic`. Empty datatypes can now be have generic `TextShow` and `TextShow1` instances.

# 2.1.2
* Fixed GHC 7.10.3 build
* Extended `TextShow.Generic` to allow `genericShowbPrec` (and friends) to be used on `Generic` datatypes with `Char#`, `Double#`, `Float#`, `Int#`, and `Word#` argument types, just like string `Show` does
* Added `Generic1` instances for `FromStringShow` and `FromTextShow`
* Added `TextShow` instances for `UChar`, `UDouble`, `UFloat`, `UInt`, and `UWord` in `TextShow.GHC.Generics`

# 2.1.1
* Restore support for `semigroups-0.16.1`

# 2.1
* Require `semigroups` >= 0.17
* Added `showsToShowb` and `showbToShows` to `TextShow`
* Added `TextShow1 FromStringShow` instance
* Added `showbGiveGCStats`, `showbDoCostCentres`, `showbDoHeapProfile`, and `showbDoTrace` to `TextShow.GHC.RTS.Flags` if using `base-4.8.2` or later
* Exported `showbDownPrecWith` on earlier versions of GHC
* Fixed mistakes in TH error output
* Make `Proxy`, `Coercion`, and `(:~:)` instances poly-kinded
* Changed implementation of `showbByteStringStrict`, `showbByteStrictLazy(Prec)`, `showbShortByteString` (in `TextShow.Data.ByteString`), `showbThreadIdPrec` (in `TextShow.Control.Concurrent`), `showbEvent`, and `showbFdKeyPrec` (in `TextShow.GHC.Event`) to avoid using `String`s as an intermediary

# 2
* Changed the top-level module name from `Text.Show.Text` to `TextShow`, since the former was extremely verbose without much benefit. As a result, this will break all existing code that depends on `text-show`.
* Several typeclasses and functions were renamed so as to not to clash with the `Prelude`:
  * `Show` → `TextShow`
  * `Show1` → `TextShow1`
  * `Show2` → `TextShow2`
  * `show` → `showt`
  * `showLazy` → `showtl`
  * `showPrec` → `showtPrec`
  * `showPrecLazy` → `showtlPrec`
  * `showList` → `showtList`
  * `showListLazy` → `showtlList`
  * `print` → `printT`
  * `printLazy` → `printTL`
  * `hPrint` → `hPrintT`
  * `hPrintLazy` → `hPrintTL`
  * `GShow` → `GTextShow`
  * `GShow1` → `GTextShow1`
  * `genericShow` → `genericShowt`
  * `genericShowLazy` → `genericShowtl`
  * `genericShowPrec` → `genericShowtPrec`
  * `genericShowPrecLazy` → `genericShowtlPrec`
  * `genericShowList` → `genericShowtList`
  * `genericShowListLazy` → `genericShowtlList`
  * `genericPrint` → `genericPrintT`
  * `genericPrintLazy` → `genericPrintTL`
  * `genericHPrint` → `genericHPrintT`
  * `genericHPrintLazy` → `genericHPrintTL`
  * `deriveShow` → `deriveTextShow`
  * `deriveShow1` → `deriveTextShow1`
  * `deriveShow2` → `deriveTextShow2`
  * `mkShow` → `makeShowt`
  * `mkShowLazy` → `makeShowtl`
  * `mkShowPrec` → `makeShowtPrec`
  * `mkShowPrecLazy` → `makeShowtlPrec`
  * `mkShowList` → `makeShowtList`
  * `mkShowListLazy` → `makeShowtlList`
  * `mkShowb` → `makeShowb`
  * `mkShowbPrec` → `makeShowbPrec`
  * `mkShowbList` → `makeShowbList`
  * `mkPrint` → `makePrintT`
  * `mkPrintLazy` → `makePrintTL`
  * `mkHPrint` → `makeHPrintT`
  * `mkHPrintLazy` → `makeHPrintTL`
  * `mkShowbPrecWith` → `makeShowbPrecWith`
  * `mkShowbPrec1` → `makeShowbPrec1`
  * `mkShowbPrecWith2` → `makeShowbPrecWith2`
  * `mkShowbPrec2` → `makeShowbPrec2`
  * `trace` → `tracet`
  * `traceLazy` → `tracetl`
  * `traceId` → `tracetId`
  * `traceIdLazy` → `tracetlId`
  * `traceShow` → `traceTextShow`
  * `traceShowId` → `traceTextShowId`
  * `traceStack` → `tracetStack`
  * `traceStackLazy` → `tracetlStack`
  * `traceIO` → `tracetIO`
  * `traceIOLazy` → `tracetlIO`
  * `traceM` → `tracetM`
  * `traceMLazy` → `tracetlM`
  * `traceShowM` → `traceTextShowM`
  * `traceEvent` → `tracetEvent`
  * `traceEventLazy` → `tracetlEvent`
  * `traceEventIO` → `travetEventIO`
  * `traceEventIOLazy` → `tracetlEventIO`
  * `traceMarker` → `tracetMarker`
  * `traceMarkerLazy` → `tracetlMarker`
  * `traceMarkerIO` → `tracetMarkerIO`
  * `traceMarkerIOLazy` → `tracetlMarkerIO`
  * `genericTraceShow` → `genericTraceTextShow`
  * `genericTraceShowId` → `genericTraceTextShowId`
  * `genericTraceShowM` → `genericTraceTextShowM`
  * `mkTraceShow` → `makeTraceTextShow`
  * `mkTraceShowId` → `makeTraceTextShowId`
  * `mkTraceShowM` → `makeTraceTextShowM`
* Added `TextShow Lifetime` instance in `TextShow.GHC.Event` (if using `base-4.8.1.0` or later)
* Generalized `tracetM`, `tracetlM`, and `traceTextShowM` to use an `Applicative` constraint instead of `Monad`
* Fixed a bug in which the `TextShow(1)` instances for `Proxy`, `(:~:)`, and `Coercion` didn't use `-XPolyKinds`
* Fixed a bug in the Template Haskell deriver which would cause `deriveTextShow` to fail on type parameters with sufficiently high kinds

# 1
* The `Show1` class has been completely overhauled. `Show1` now uses the function `showbPrecWith`, which takes as an argument a function of type `Int -> a -> Builder` to show occurrences of the type parameter (instead of requiring the type parameter to be a `Show` instance). This matches the new implementation of `Show1` in the next version of `transformers`. A similar `Show2` class (with the function `showbPrecWith2`) was also added.
* As a consequence, `Show1` instances should no longer be defined in terms of `showbPrec`; rather, `Show` instances should be defined in terms of `showbPrecWith` or `showbPrecWith2`, and `Show1` instances can be defined in terms of `showbPrecWith2`.
* The `showbPrec1` function is no longer a class method of `Show1`, but is now a standalone function defined in terms of `showbPrecWith`. `showbPrec1` can be useful for defining `Show` instances. A similar `showbPrec2` function was also added.
* The monomorphic functions in the many submodules of this package have been generalized (where possible) to use `Show1` and `Show2` instances. These functions have `-PrecWith` and `-PrecWith2` suffixes, respectively.
* Because of the generality of the new `showPrecWith` function, `Show1` instances are now possible for `Ratio`, `Alt`, `Rec1`, `M1`, `(:+:)`, `(:*:)`, and `(:.:)`.
* Removed many silly instances for `FromStringShow` and `FromTextShow`, since they'll never be used in the ways suggested by those instances to begin with.
* The Template Haskell engine has been completely overhauled. Deriving `Show1` and `Show2` instances are now possible using the `deriveShow1` and `deriveShow2` functions. See the documentation in `Text.Show.Text.TH` for more details. In addition, the `mkShowbPrecWith`, `mkShowbPrec1`, `mkShowbPrecWith2`, and `mkShowbPrec2` functions were added.
* Removed the ability to call `deriveShow` or `mkShowbPrec` (or other functions prefixed with `mk-`) using a data family name. This is considered a misfeature. If you want to derive `Show` for data family instances, use the corresponding `data instance` or `newtype instance` constructor name as an argument instead.
 * Removed `PragmaOptions`, `deriveShowPragmas`, `defaultInlineShowbPrec`, `defaultInlineShowb`, and `defaultInlineShowbList`, as it was impossible to make Template Haskell-generated pragmas work consistently across different versions of GHC. If you really want to use `INLINE` and `SPECIALIZE instance` pragmas with your Template Haskell-generated code, create manual instances with `mkShowbPrec` and family.
* `Show1` instances can now be created generically using the `genericShowbPrecWith` function in `Text.Show.Text.Generics`. A `genericShowbPrec1` was also added.
* Added `generic-deriving` as a dependency, which allows generics-related code to be exported on more versions of GHC
* `ConType` (in `Text.Show.Text.Generics`) now has an `Inf String` constructor instead of `Inf Builder`. As a result, `ConType` now always an `Eq` and `Ord` instance, and a `Read ConType` instance was added.
* `Typeable` instances for the promoted data constructors `'FromStringShow` and `'FromTextShow`
* Added `showbFPFormat` to `Text.Show.Text.Data.Floating`
* Revamped test suite

### 0.8.1.1
* Retroactive `CHANGELOG` update

## 0.8.1
* Fix test suite build with older versions of `QuickCheck`

# 0.8
* Exported `formatRealFloatB` and `formatRealFloatAltB` from `Text.Show.Text.Data.Floating`. Reexported `FPFormat` (from `text`) in the same module, and added a `Text` `Show` instance for it.
* The `Show` instance for `Ratio a` now only requires a `Show a` constraint if using `base-4.4.0.0` or later (if using `base-4.3.0.0`, it requires a `(Show a, Integral a)` constraint)
* Added `showbSingPrec` to `Text.Show.Text.GHC.TypeLits` (if using `base-4.6`)
* Modules which were previously exported only if using a recent-enough version of GHC/`base` (e.g., `Text.Show.Text.GHC.Generics`) are now always exposed. If the functionality that the module provides is not available on a given version of GHC/`base`, the module will not expose anything.
* Bump lower version bounds of `text` to 0.11.1 due to reexporting `FPFormat`
* Added `showbUnicodeException`, `showbI16Prec`, `showbDecodingPrec`, and `showbSizePrec` functions (and corresponding `Show` instances) to `Text.Show.Text.Data.Text`
* Made `GShow` in `Text.Show.Text.Generics` poly-kinded
* The Template Haskell deriver (and `GShow`) now handles "infix" data constructors that are applied as prefix correctly (e.g., `data Amp a = (:&) a a`)
* The Template Haskell deriver now handles showable unlifted types (`Char#`, `Double#`, `Float#`, `Int#`, and `Word#`) correctly on GHC 7.11 and later
* The Template Haskell derive now does not parenthesize record types regardless of precedence on GHC 7.11 and later
* Fixed build on GHC 7.2
* Changed test-suite to use `hspec`, which allows for it to be built on GHC 7.0 and 7.2

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
* Removed the phantom-type detecting mechanism with `template-haskell-2.9.0.0` or higher. This method of finding phantom types is intrinsically flawed and is not usable on older GHCs.
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
