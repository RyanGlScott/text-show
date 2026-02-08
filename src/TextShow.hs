{-|
Module:      TextShow
Copyright:   (C) 2014-2017 Ryan Scott
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
    , showtParen
    , showtlParen
    , showbCommaSpace
    , showtCommaSpace
    , showtlCommaSpace
    , showbSpace
    , showtSpace
    , showtlSpace
      -- ** 'TextShow1'
    , TextShow1(..)
    , showbPrec1
    , showbUnaryWith
    , liftShowtPrec
    , liftShowtlPrec
      -- ** 'TextShow2'
    , TextShow2(..)
    , showbPrec2
    , showbBinaryWith
    , liftShowtPrec2
    , liftShowtlPrec2
      -- * 'Builder's
      -- ** The 'Builder' type
    , Builder
    , runBuilder
    , runBuilderLazy
    , runBuilderString
      -- ** Constructing 'Builder's
    , fromAddr
    , fromChar
    , fromText
    , fromLazyText
    , fromString
      -- ** 'Builder' utility functions
    , lengthB
    , unlinesB
    , unwordsB
      -- * Printing values
    , printT
    , printTL
    , hPrintT
    , hPrintTL
      -- * Conversions
      -- ** Conversion between 'TextShow' and string 'Show'
    , FromStringShow(..)
    , FromTextShow(..)
    , FromStringShow1(..)
    , FromTextShow1(..)
    , FromStringShow2(..)
    , FromTextShow2(..)
    , showsPrecToShowbPrec
    , showsToShowb
    , showbPrecToShowsPrec
    , showbToShows
      -- ** Conversions between 'Builder', strict 'TS.Text', and lazy 'TL.Text'
    , showtPrecToShowbPrec
    , showtlPrecToShowbPrec
    , showtToShowb
    , showtlToShowb
    , showbPrecToShowtPrec
    , showbPrecToShowtlPrec
    , showbToShowt
    , showbToShowtl
    ) where

import qualified Data.Text as TS ()
import qualified Data.Text.Lazy as TL ()
-- TODO RGS: Is it really worth it to re-export all this stuff?
import           Data.Text.Builder.Linear (Builder, fromAddr, fromChar,
                                           fromText, runBuilder)

import           Prelude ()

import           TextShow.Classes
import           TextShow.FromStringTextShow
import           TextShow.Instances ()
import           TextShow.Utils (fromLazyText, fromString, lengthB,
                                 runBuilderLazy, runBuilderString,
                                 unlinesB, unwordsB)
