{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.Text.Read
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' function for 'Lexeme' (and 'Number', if using a
recent-enough version of @base@).

/Since: 0.3/
-}
module Text.Show.Text.Text.Read (
      showbLexemePrec
#if MIN_VERSION_base(4,7,0)
    , showbNumberPrec
#endif
    ) where

import Data.Text.Lazy.Builder (Builder)

import Prelude hiding (Show)

import Text.Read.Lex (Lexeme(..))
#if MIN_VERSION_base(4,7,0)
import Text.Read.Lex (Number)
#endif
import Text.Show.Text.Classes (Show(showbPrec), showbUnaryWith)
#if MIN_VERSION_base(4,6,0)
import Text.Show.Text.Classes (FromStringShow(..))
#else
import Text.Show.Text.Data.Integral ()
import Text.Show.Text.Data.Ratio ()
#endif
import Text.Show.Text.Data.Char ()
import Text.Show.Text.Data.List ()

#include "inline.h"

-- | Convert a 'Lexeme' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.3/
showbLexemePrec :: Int -> Lexeme -> Builder
showbLexemePrec p (Char c)   = showbUnaryWith showbPrec "Char"   p c
showbLexemePrec p (String s) = showbUnaryWith showbPrec "String" p s
showbLexemePrec p (Punc pun) = showbUnaryWith showbPrec "Punc"   p pun
showbLexemePrec p (Ident i)  = showbUnaryWith showbPrec "Ident"  p i
showbLexemePrec p (Symbol s) = showbUnaryWith showbPrec "Symbol" p s
#if MIN_VERSION_base(4,6,0)
showbLexemePrec p (Number n) = showbUnaryWith showbPrec "Number" p $ FromStringShow n
#else
showbLexemePrec p (Int i)    = showbUnaryWith showbPrec "Int" p i
showbLexemePrec p (Rat r)    = showbUnaryWith showbPrec "Rat" p r
#endif
showbLexemePrec _ EOF        = "EOF"

#if MIN_VERSION_base(4,7,0)
-- | Convert a 'Number' to a 'Builder' with the given precedence.
-- This function is only available with @base-4.7.0.0@ or later.
-- 
-- /Since: 0.3/
showbNumberPrec :: Int -> Number -> Builder
showbNumberPrec p = showbPrec p . FromStringShow
{-# INLINE showbNumberPrec #-}
#endif

instance Show Lexeme where
    showbPrec = showbLexemePrec
    INLINE_INST_FUN(showbPrec)

#if MIN_VERSION_base(4,7,0)
instance Show Number where
    showbPrec = showbNumberPrec
    {-# INLINE showbPrec #-}
#endif
