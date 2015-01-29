# `text-show` [![Hackage version](https://img.shields.io/hackage/v/text-show.svg?style=flat)](http://hackage.haskell.org/package/text-show) [![Build Status](https://img.shields.io/travis/RyanGlScott/text-show.svg?style=flat)](https://travis-ci.org/RyanGlScott/text-show)

`text-show` offers a replacement for the `Show` typeclass intended for use with `Text` instead of `String`s. This package was created in the spirit of [`bytestring-show`](http://hackage.haskell.org/package/bytestring-show).

At the moment, `text-show` provides `Show` instances for most data types in the [`array`](http://hackage.haskell.org/package/array), [`base`](http://hackage.haskell.org/package/base), [`bytestring`](http://hackage.haskell.org/package/bytestring), and [`text`](http://hackage.haskell.org/package/text) packages. Therefore, much of the source code for `text-show` consists of borrowed code from those packages in order to ensure that the behaviors of the two `Show` typeclasses coincide.

For most uses, simply importing `Text.Show.Text` will suffice:

```haskell
module Main where

import Data.Text (Text)
import Prelude hiding (Show(..), print)
import Text.Show.Text

number :: Text
number = show (Just "Hello, World!")

main :: IO ()
main = print number
```

If you desire it, there are also monomorphic versions of the `showb` function available in the submodules of `Text.Show.Text`. A naming convention is present in which functions that show different values depending on the precedence end with `Prec`(e.g., `showbIntPrec`), whereas functions that show the same values regardless of precedence do not end with `Prec` (e.g., `showbBool`).
