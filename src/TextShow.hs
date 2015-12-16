{-|
Module:      TextShow
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Efficiently convert from values to 'Text' via 'Builder's.

/Since: 2/
-}
module TextShow (
      -- * The @TextShow@ classes
      -- ** 'TextShow'
      TextShow(..)
    , showbParen
    , showbSpace
      -- ** 'TextShow1'
    , TextShow1(..)
    , showbPrec1
    , showbUnaryWith
      -- ** 'TextShow2'
    , TextShow2(..)
    , showbPrec2
    , showbBinaryWith
      -- * 'Builder's
      -- ** The 'Builder' type
    , Builder
    , toText
    , toLazyText
    , toLazyTextWith
    , toString
      -- ** Constructing 'Builder's
    , singleton
    , fromText
    , fromLazyText
    , fromString
      -- ** Flushing the buffer state
    , flush
      -- ** 'Builder' utility functions
    , lengthB
    , unlinesB
    , unwordsB
      -- * Printing values
    , printT
    , printTL
    , hPrintT
    , hPrintTL
      -- * Conversion between 'TextShow' and string @Show@
    , FromStringShow(..)
    , FromTextShow(..)
    , showsToShowb
    , showbToShows
    ) where

import Data.Text.Lazy.Builder

import Prelude ()

import TextShow.Classes
import TextShow.FromStringTextShow
import TextShow.Instances ()
import TextShow.Utils (toString, toText, lengthB, unlinesB, unwordsB)
