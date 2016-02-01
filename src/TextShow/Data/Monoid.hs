{-# LANGUAGE CPP              #-}
{-# LANGUAGE TemplateHaskell  #-}
#if MIN_VERSION_base(4,8,0)
{-# LANGUAGE FlexibleContexts #-}
#endif
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      TextShow.Data.Monoid
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Monomorphic 'Show' functions for 'Monoid'-related newtypes.

/Since: 2/
-}
module TextShow.Data.Monoid (
      showbAllPrec
    , showbAnyPrec
    , liftShowbDualPrec
    , liftShowbFirstPrec
    , liftShowbLastPrec
    , liftShowbProductPrec
    , liftShowbSumPrec
#if MIN_VERSION_base(4,8,0)
    , showbAltPrec
    , liftShowbAltPrec
#endif
    ) where

import Data.Monoid.Compat (All, Any, Dual, First, Last, Product, Sum)
import Data.Text.Lazy.Builder (Builder)

import TextShow.Classes (TextShow(..), TextShow1(..))
import TextShow.Data.Bool ()
import TextShow.Data.Maybe ()
import TextShow.TH.Internal (deriveTextShow, deriveTextShow1)

#if MIN_VERSION_base(4,8,0)
import Data.Monoid (Alt)
import TextShow.TH.Internal (makeShowbPrec)
#endif

#include "inline.h"

-- | Convert an 'All' value to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbAllPrec :: Int -> All -> Builder
showbAllPrec = showbPrec
{-# INLINE showbAllPrec #-}

-- | Convert an 'Any' value to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbAnyPrec :: Int -> Any -> Builder
showbAnyPrec = showbPrec
{-# INLINE showbAnyPrec #-}

-- | Convert a 'Dual' value to a 'Builder' with the given show function and precedence.
--
-- /Since: 3/
liftShowbDualPrec :: (Int -> a -> Builder) -> Int -> Dual a -> Builder
liftShowbDualPrec sp = liftShowbPrec sp undefined
{-# INLINE liftShowbDualPrec #-}

-- | Convert a 'First' value to a 'Builder' with the given show function and precedence.
--
-- /Since: 3/
liftShowbFirstPrec :: (Int -> a -> Builder) -> Int -> First a -> Builder
liftShowbFirstPrec sp = liftShowbPrec sp undefined
{-# INLINE liftShowbFirstPrec #-}

-- | Convert a 'Last' value to a 'Builder' with the given show function and precedence.
--
-- /Since: 3/
liftShowbLastPrec :: (Int -> a -> Builder) -> Int -> Last a -> Builder
liftShowbLastPrec sp = liftShowbPrec sp undefined
{-# INLINE liftShowbLastPrec #-}

-- | Convert a 'Product' value to a 'Builder' with the given show function
-- and precedence.
--
-- /Since: 3/
liftShowbProductPrec :: (Int -> a -> Builder) -> Int -> Product a -> Builder
liftShowbProductPrec sp = liftShowbPrec sp undefined
{-# INLINE liftShowbProductPrec #-}

-- | Convert a 'Sum' value to a 'Builder' with the given show function and precedence.
--
-- /Since: 3/
liftShowbSumPrec :: (Int -> a -> Builder) -> Int -> Sum a -> Builder
liftShowbSumPrec sp = liftShowbPrec sp undefined
{-# INLINE liftShowbSumPrec #-}

#if MIN_VERSION_base(4,8,0)
-- | Convert an 'Alt' value to a 'Builder' with the given precedence.
-- This function is only available with @base-4.8.0.0@ or later.
--
-- /Since: 2/
showbAltPrec :: TextShow (f a) => Int -> Alt f a -> Builder
showbAltPrec = showbPrec
{-# INLINE showbAltPrec #-}

-- | Convert an 'Alt' value to a 'Builder' with the given show function and precedence.
-- This function is only available with @base-4.8.0.0@ or later.
--
-- /Since: 3/
liftShowbAltPrec :: TextShow1 f => (Int -> a -> Builder) -> ([a] -> Builder)
                 -> Int -> Alt f a -> Builder
liftShowbAltPrec = liftShowbPrec
{-# INLINE liftShowbAltPrec #-}
#endif

$(deriveTextShow  ''All)
$(deriveTextShow  ''Any)
$(deriveTextShow  ''Dual)
$(deriveTextShow1 ''Dual)
$(deriveTextShow  ''First)
$(deriveTextShow1 ''First)
$(deriveTextShow  ''Last)
$(deriveTextShow1 ''Last)
$(deriveTextShow  ''Product)
$(deriveTextShow1 ''Product)
$(deriveTextShow  ''Sum)
$(deriveTextShow1 ''Sum)

#if MIN_VERSION_base(4,8,0)
instance TextShow (f a) => TextShow (Alt f a) where
    showbPrec = $(makeShowbPrec ''Alt)

$(deriveTextShow1 ''Alt)
#endif
