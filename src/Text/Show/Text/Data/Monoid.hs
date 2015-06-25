{-# LANGUAGE CPP              #-}
{-# LANGUAGE TemplateHaskell  #-}
#if MIN_VERSION_base(4,8,0)
{-# LANGUAGE FlexibleContexts #-}
#endif
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.Data.Monoid
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' functions for 'Monoid'-related newtypes.

/Since: 0.3/
-}
module Text.Show.Text.Data.Monoid (
      showbAllPrec
    , showbAnyPrec
    , showbDualPrecWith
    , showbFirstPrecWith
    , showbLastPrecWith
    , showbProductPrecWith
    , showbSumPrecWith
#if MIN_VERSION_base(4,8,0)
    , showbAltPrec
    , showbAltPrecWith
#endif
    ) where

import Data.Monoid.Compat (All, Any, Dual, First, Last, Product, Sum)
import Data.Text.Lazy.Builder (Builder)

import Prelude hiding (Show)

import Text.Show.Text.Classes (Show(showbPrec), showbPrecWith)
import Text.Show.Text.Data.Bool ()
import Text.Show.Text.Data.Maybe ()
import Text.Show.Text.TH.Internal (deriveShow, deriveShow1)

#if MIN_VERSION_base(4,8,0)
import Data.Monoid (Alt)
import Text.Show.Text.Classes (Show1)
import Text.Show.Text.TH.Internal (mkShowbPrec)
#endif

#include "inline.h"

-- | Convert an 'All' value to a 'Builder' with the given precedence.
--
-- /Since: 0.3/
showbAllPrec :: Int -> All -> Builder
showbAllPrec = showbPrec
{-# INLINE showbAllPrec #-}

-- | Convert an 'Any' value to a 'Builder' with the given precedence.
--
-- /Since: 0.3/
showbAnyPrec :: Int -> Any -> Builder
showbAnyPrec = showbPrec
{-# INLINE showbAnyPrec #-}

-- | Convert a 'Dual' value to a 'Builder' with the given show function and precedence.
--
-- /Since: 1/
showbDualPrecWith :: (Int -> a -> Builder) -> Int -> Dual a -> Builder
showbDualPrecWith = showbPrecWith
{-# INLINE showbDualPrecWith #-}

-- | Convert a 'First' value to a 'Builder' with the given show function and precedence.
--
-- /Since: 1/
showbFirstPrecWith :: (Int -> a -> Builder) -> Int -> First a -> Builder
showbFirstPrecWith = showbPrecWith
{-# INLINE showbFirstPrecWith #-}

-- | Convert a 'Last' value to a 'Builder' with the given show function and precedence.
--
-- /Since: 1/
showbLastPrecWith :: (Int -> a -> Builder) -> Int -> Last a -> Builder
showbLastPrecWith = showbPrecWith
{-# INLINE showbLastPrecWith #-}

-- | Convert a 'Product' value to a 'Builder' with the given show function
-- and precedence.
--
-- /Since: 1/
showbProductPrecWith :: (Int -> a -> Builder) -> Int -> Product a -> Builder
showbProductPrecWith = showbPrecWith
{-# INLINE showbProductPrecWith #-}

-- | Convert a 'Sum' value to a 'Builder' with the given show function and precedence.
--
-- /Since: 1/
showbSumPrecWith :: (Int -> a -> Builder) -> Int -> Sum a -> Builder
showbSumPrecWith = showbPrecWith
{-# INLINE showbSumPrecWith #-}

#if MIN_VERSION_base(4,8,0)
-- | Convert an 'Alt' value to a 'Builder' with the given precedence.
-- This function is only available with @base-4.8.0.0@ or later.
--
-- /Since: 0.8/
showbAltPrec :: Show (f a) => Int -> Alt f a -> Builder
showbAltPrec = showbPrec
{-# INLINE showbAltPrec #-}

-- | Convert an 'Alt' value to a 'Builder' with the given show function and precedence.
-- This function is only available with @base-4.8.0.0@ or later.
--
-- /Since: 1/
showbAltPrecWith :: Show1 f => (Int -> a -> Builder) -> Int -> Alt f a -> Builder
showbAltPrecWith = showbPrecWith
#endif

$(deriveShow  ''All)
$(deriveShow  ''Any)
$(deriveShow  ''Dual)
$(deriveShow1 ''Dual)
$(deriveShow  ''First)
$(deriveShow1 ''First)
$(deriveShow  ''Last)
$(deriveShow1 ''Last)
$(deriveShow  ''Product)
$(deriveShow1 ''Product)
$(deriveShow  ''Sum)
$(deriveShow1 ''Sum)

#if MIN_VERSION_base(4,8,0)
instance Show (f a) => Show (Alt f a) where
    showbPrec = $(mkShowbPrec ''Alt)

$(deriveShow1 ''Alt)
#endif
