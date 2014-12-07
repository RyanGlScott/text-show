{-# LANGUAGE CPP, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Show.Text.Text.Read
-- Copyright   :  (C) 2014 Ryan Scott
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  Experimental
-- Portability :  GHC
-- 
-- Monomorphic 'Show' function for 'Lexeme' (and 'Number', if using a
-- recent-enough version of @base@).
----------------------------------------------------------------------------
module Text.Show.Text.Text.Read (
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

import Text.Show.Text.Class (showbPrec)
import Text.Show.Text.Data.Char ()
import Text.Show.Text.Data.Integral ()
import Text.Show.Text.Data.List ()
import Text.Show.Text.Data.Maybe ()
import Text.Show.Text.TH.Internal (deriveShow)

-- | Convert a 'Lexeme' to a 'Builder' with the given precedence.
showbLexemePrec :: Int -> Lexeme -> Builder
showbLexemePrec = showbPrec
{-# INLINE showbLexemePrec #-}

#if MIN_VERSION_base(4,7,0)
-- | Convert a 'Number' to a 'Builder' with the given precedence.
showbNumberPrec :: Int -> Number -> Builder
showbNumberPrec = showbPrec
{-# INLINE showbNumberPrec #-}
#endif

$(deriveShow ''Lexeme)
#if MIN_VERSION_base(4,7,0)
$(deriveShow ''Number)
#endif
