{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.GHC.Event
Copyright:   (C) 2014 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' functions for data types in the @Event@ module.
-}
module Text.Show.Text.GHC.Event (
#if defined(__GHCJS__)
    ) where
#else
      showbEvent
    , showbFdKeyPrec
    ) where 

import Data.Text.Lazy.Builder (Builder)
import GHC.Event (Event, FdKey)
import Prelude hiding (Show)
import Text.Show.Text.Classes (Show(showb, showbPrec), FromStringShow(..))

-- | Convert an 'Event' to a 'Builder'.
showbEvent :: Event -> Builder
showbEvent = showb . FromStringShow
{-# INLINE showbEvent #-}

-- | Convert an 'FdKey' to a 'Builder' with the given precedence.
showbFdKeyPrec :: Int -> FdKey -> Builder
showbFdKeyPrec p = showbPrec p . FromStringShow
{-# INLINE showbFdKeyPrec #-}

instance Show Event where
    showb = showbEvent
    {-# INLINE showb #-}

instance Show FdKey where
    showbPrec = showbFdKeyPrec
    {-# INLINE showbPrec #-}
#endif
