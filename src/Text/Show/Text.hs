{-|
Module:      Text.Show.Text
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Efficiently convert from values to 'Text' via 'Builder's.

/Since: 0.1/
-}
module Text.Show.Text (
      -- * The @Show@-related classes
      -- ** 'Show'
      Show(..)
    , show
    , showLazy
    , showPrec
    , showPrecLazy
    , showList
    , showListLazy
    , showbParen
    , showbSpace
      -- ** 'Show1'
    , Show1(..)
    , showbPrec1
    , showbUnaryWith
      -- ** 'Show2'
    , Show2(..)
    , showbPrec2
    , showbBinaryWith
      -- * 'Builder's
    , module Data.Text.Lazy.Builder
    , toString
    , toText
    , lengthB
    , unlinesB
    , unwordsB
      -- * Printing values
    , print
    , printLazy
    , hPrint
    , hPrintLazy
      -- * Conversion between @String@ and @Text@ 'Show'
    , FromStringShow(..)
    , FromTextShow(..)
    ) where

import Data.Text.Lazy.Builder

import Prelude ()

import Text.Show.Text.Classes
import Text.Show.Text.Instances ()
import Text.Show.Text.Utils (toString, toText, lengthB,
                             unlinesB, unwordsB)
