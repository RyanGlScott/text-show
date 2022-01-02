{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
#if __GLASGOW_HASKELL__ == 800
-- See Note [Increased simpl-tick-factor on old GHCs]
{-# OPTIONS_GHC -fsimpl-tick-factor=200 #-}
#endif

{-|
Module:      TextShow.Data.Ratio
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'TextShow' instances for 'Ratio'.

/Since: 2/
-}
module TextShow.Data.Complex () where

import Data.Complex (Complex)

import TextShow.Classes (TextShow(..))
import TextShow.Data.Floating ()
import TextShow.TH.Internal (deriveTextShow1, makeShowbPrec)

-- | /Since: 2/
instance TextShow a => TextShow (Complex a) where
    {-# SPECIALIZE instance TextShow (Complex Float)  #-}
    {-# SPECIALIZE instance TextShow (Complex Double) #-}
    showbPrec = $(makeShowbPrec ''Complex)
    {-# INLINE showbPrec #-}

-- | /Since: 2/
$(deriveTextShow1 ''Complex)

{-
Note [Increased simpl-tick-factor on old GHCs]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Compiling certain text-show modules with optimizations on old versions of GHC
(particularly 8.0 and 8.2) will trigger "Simplifier ticks exhausted" panics.
To make things worse, this sometimes depends on whether a certain version of
the text library is being used. There are two possible ways to work around
this issue:

1. Figure out which uses of the INLINE pragma in text-show are responsible
   and remove them.
2. Just increase the tick limit.

Since executing on (1) will require a lot of effort to fix an issue that only
happens on old versions of GHC, I've opted for the simple solution of (2) for
now. Issue #51 is a reminder to revisit this choice.
-}
