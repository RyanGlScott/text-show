{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      TextShow.Text.Read
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Monomorphic 'TextShow' function for 'Lexeme' (and 'Number', if using a
recent-enough version of @base@).

/Since: 2/
-}
module TextShow.Text.Read (
      showbLexemePrec
#if MIN_VERSION_base(4,7,0)
    , showbNumberPrec
#endif
    ) where

import Data.Text.Lazy.Builder (Builder)

import Text.Read.Lex (Lexeme)
#if MIN_VERSION_base(4,7,0)
import Text.Read.Lex (Number)
#endif

import TextShow.Classes (showbPrec)
import TextShow.Data.Char     ()
import TextShow.Data.Integral ()
import TextShow.Data.List     ()
import TextShow.Data.Maybe    ()
import TextShow.Data.Ratio    ()
import TextShow.TH.Internal (deriveTextShow)
#if MIN_VERSION_base(4,6,0)
import TextShow.TH.Names (numberTypeName)
#endif

#include "inline.h"

-- | Convert a 'Lexeme' to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbLexemePrec :: Int -> Lexeme -> Builder
showbLexemePrec = showbPrec
{-# INLINE showbLexemePrec #-}

#if MIN_VERSION_base(4,7,0)
-- | Convert a 'Number' to a 'Builder' with the given precedence.
-- This function is only available with @base-4.7.0.0@ or later.
--
-- /Since: 2/
showbNumberPrec :: Int -> Number -> Builder
showbNumberPrec = showbPrec
{-# INLINE showbNumberPrec #-}
#endif

$(deriveTextShow ''Lexeme)
#if MIN_VERSION_base(4,6,0)
$(deriveTextShow numberTypeName)
#endif
