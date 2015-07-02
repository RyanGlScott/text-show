{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.Data.Ratio
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Monomorphic 'Show' function for 'Ratio' values.

Due to use of the @DatatypeContexts@ extension, there is no @Show1 Complex@
instance on @base-4.3.0.0@.

/Since: 0.5/
-}
module Text.Show.Text.Data.Complex (showbComplexPrec) where

import Data.Complex (Complex)
import Data.Text.Lazy.Builder (Builder)

import Prelude hiding (Show)

import Text.Show.Text.Classes (Show(showbPrec))
import Text.Show.Text.Data.Floating ()
import Text.Show.Text.TH.Internal (mkShowbPrec)
#if MIN_VERSION_base(4,4,0)
import Text.Show.Text.TH.Internal (deriveShow1)
#endif

#include "inline.h"

-- | Convert a 'Complex' value to a 'Builder' with the given precedence.
--
-- Note that on @base-4.3.0.0@, this function must have a @('Show' a, 'RealFloat' a)@
-- constraint instead of just a @('Show' a)@ constraint.
-- /Since: 0.5/
#if MIN_VERSION_base(4,4,0)
showbComplexPrec :: Show a
#else
showbComplexPrec :: (RealFloat a, Show a)
#endif
                 => Int
                 -> Complex a
                 -> Builder
showbComplexPrec = showbPrec
{-# INLINE showbComplexPrec #-}

instance
#if MIN_VERSION_base(4,4,0)
  Show a
#else
  (RealFloat a, Show a)
#endif
  => Show (Complex a) where
    {-# SPECIALIZE instance Show (Complex Float)  #-}
    {-# SPECIALIZE instance Show (Complex Double) #-}
    showbPrec = $(mkShowbPrec ''Complex)
    INLINE_INST_FUN(showbPrec)

#if MIN_VERSION_base(4,4,0)
$(deriveShow1 ''Complex)
#endif
