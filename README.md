# `text-show`
[![Hackage](https://img.shields.io/hackage/v/text-show.svg)][Hackage: text-show]
[![Hackage Dependencies](https://img.shields.io/hackage-deps/v/text-show.svg)](http://packdeps.haskellers.com/reverse/text-show)
[![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-blue.svg)][Haskell.org]
[![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)][tl;dr Legal: BSD3]
[![Linux build](https://github.com/RyanGlScott/text-show/workflows/Haskell-CI/badge.svg)](https://github.com/RyanGlScott/text-show/actions?query=workflow%3AHaskell-CI)
[![Windows build](https://ci.appveyor.com/api/projects/status/fy1q86lbfttmnthy?svg=true)](https://ci.appveyor.com/project/RyanGlScott/text-show)

[Hackage: text-show]:
  http://hackage.haskell.org/package/text-show
  "text-show package on Hackage"
[Haskell.org]:
  http://www.haskell.org
  "The Haskell Programming Language"
[tl;dr Legal: BSD3]:
  https://tldrlegal.com/license/bsd-3-clause-license-%28revised%29
  "BSD 3-Clause License (Revised)"

`text-show` offers a replacement for the `Show` typeclass intended for use with `Text` instead of `String`s. This package was created in the spirit of [`bytestring-show`](http://hackage.haskell.org/package/bytestring-show).

For most uses, simply importing `TextShow` will suffice:

```haskell
module Main where

import TextShow

main :: IO ()
main = printT (Just "Hello, World!")
```

See also the [naming conventions](https://github.com/RyanGlScott/text-show/wiki/Naming-conventions) page.

Support for automatically deriving `TextShow` instances can be found in the `TextShow.TH` and `TextShow.Generic` modules.

## Scope of the library

`text-show` only provides instances for data types in the following packages:

* [`array`](http://hackage.haskell.org/package/array)
* [`base`](http://hackage.haskell.org/package/base)
* [`bytestring`](http://hackage.haskell.org/package/bytestring)
* [`text`](http://hackage.haskell.org/package/text)

This policy is in place to keep `text-show`'s dependencies reasonably light. If
you need a `TextShow` instance for a library that is not in this list, it may
be covered by the
[`text-show-instances`](https://github.com/RyanGlScott/text-show-instances)
library.
