{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Show.Text.GHC.Stats
-- Copyright   :  (C) 2014 Ryan Scott
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  Experimental
-- Portability :  GHC
-- 
-- Monomorphic 'Show' functions for data types in the @Event@ module.
----------------------------------------------------------------------------
module Text.Show.Text.GHC.Event (showbEvent, showbFdKeyPrec) where 

import           Data.Text.Lazy.Builder (Builder, fromString)

import           GHC.Event (Event, FdKey)

import qualified Prelude as P
import           Prelude hiding (Show)

import           Text.Show.Text.Class (Show(showb, showbPrec))

-- | Convert an 'Event' to a 'Builder'.
showbEvent :: Event -> Builder
showbEvent = fromString . P.show
{-# INLINE showbEvent #-}

-- | Convert an 'FdKey' to a 'Builder' with the given precedence.
showbFdKeyPrec :: Int -> FdKey -> Builder
showbFdKeyPrec p fdKey = fromString $ P.showsPrec p fdKey ""
{-# INLINE showbFdKeyPrec #-}

instance Show Event where
    showb = showbEvent
    {-# INLINE showb #-}

instance Show FdKey where
    showbPrec = showbFdKeyPrec
    {-# INLINE showbPrec #-}