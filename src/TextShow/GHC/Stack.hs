{-# LANGUAGE CPP             #-}

#if MIN_VERSION_base(4,8,1)
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
#endif
{-|
Module:      TextShow.GHC.Stack
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Monomorphic 'TextShow' functions for 'CallStack' and 'SrcLoc' values.
This module only exports functions if using @base-4.8.1.0@ or later.

/Since: 3.0.1/
-}
module TextShow.GHC.Stack (
#if !(MIN_VERSION_base(4,8,1))
    ) where
#else
      showbCallStackPrec
    , showbSrcLocPrec
    ) where

import Data.Text.Lazy.Builder (Builder)

import GHC.Stack (CallStack)
# if MIN_VERSION_base(4,9,0)
import GHC.Stack (SrcLoc, getCallStack)
# else
import GHC.SrcLoc (SrcLoc)
# endif

import TextShow.Classes (TextShow(..))
import TextShow.Data.Char     ()
import TextShow.Data.Integral ()
import TextShow.Data.List     ()
import TextShow.Data.Tuple    ()
import TextShow.TH.Internal (deriveTextShow)

-- | Convert a 'CallStack' to a 'Builder' with the given precedence.
-- This function is only available with @base-4.8.1.0@ or later.
-- With @base-4.9@ or later, this function ignores the precedence argument.
--
-- /Since: 3.0.1/
showbCallStackPrec :: Int -> CallStack -> Builder
# if MIN_VERSION_base(4,9,0)
showbCallStackPrec _ = showb . getCallStack
# else
showbCallStackPrec = showbPrec
# endif
{-# INLINE showbCallStackPrec #-}

-- | Convert a 'SrcLoc' to a 'Builder' with the given precedence.
-- This function is only available with @base-4.8.1.0@ or later.
--
-- /Since: 3.0.1/
showbSrcLocPrec :: Int -> SrcLoc -> Builder
showbSrcLocPrec = showbPrec
{-# INLINE showbSrcLocPrec #-}

# if MIN_VERSION_base(4,9,0)
instance TextShow CallStack where
    showb = showbCallStackPrec 0
    {-# INLINE showb #-}
# else
$(deriveTextShow ''CallStack)
# endif

$(deriveTextShow ''SrcLoc)
#endif
