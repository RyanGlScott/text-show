{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      TextShow.Data.Ratio
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'TextShow' instances for 'Ratio'.

Due to use of the @DatatypeContexts@ extension, there is no @TextShow1 Complex@
instance on @base-4.3.0.0@.

/Since: 2/
-}
module TextShow.Data.Complex () where

import Data.Complex (Complex)

import TextShow.Classes (TextShow(..))
import TextShow.Data.Floating ()
import TextShow.TH.Internal (makeShowbPrec)
#if MIN_VERSION_base(4,4,0)
import TextShow.TH.Internal (deriveTextShow1)
#endif

#include "inline.h"

-- | Note that on @base-4.3.0.0@, this must have a @('TextShow' a,
-- 'RealFloat' a)@ constraint instead of just a @('TextShow' a)@ constraint.
--
-- /Since: 2/
instance
#if MIN_VERSION_base(4,4,0)
  TextShow a
#else
  (RealFloat a, TextShow a)
#endif
  => TextShow (Complex a) where
    {-# SPECIALIZE instance TextShow (Complex Float)  #-}
    {-# SPECIALIZE instance TextShow (Complex Double) #-}
    showbPrec = $(makeShowbPrec ''Complex)
    INLINE_INST_FUN(showbPrec)

#if MIN_VERSION_base(4,4,0)
-- | Only available with @base-4.4.0.0@ or later.
--
-- /Since: 2/
$(deriveTextShow1 ''Complex)
#endif
