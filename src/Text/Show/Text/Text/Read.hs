{-# LANGUAGE CPP, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.Text.Read
Copyright:   (C) 2014 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' function for 'Lexeme' (and 'Number', if using a
recent-enough version of @base@).
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
import Text.Show.Text.Classes (Show(showbPrec), showbUnary)
import Text.Show.Text.Newtypes (LitChar(..), LitString(..))
#if MIN_VERSION_base(4,6,0)
import Text.Show.Text.Newtypes (FromStringShow(..))
#else
import Text.Show.Text.Data.Integral ()
import Text.Show.Text.Data.Ratio ()
#endif

#include "inline.h"

-- | Convert a 'Lexeme' to a 'Builder' with the given precedence.
showbLexemePrec :: Int -> Lexeme -> Builder
showbLexemePrec p (Char c)   = showbUnary "Char"   p $ LitChar c
showbLexemePrec p (String s) = showbUnary "String" p $ LitString s
showbLexemePrec p (Punc pun) = showbUnary "Punc"   p $ LitString pun
showbLexemePrec p (Ident i)  = showbUnary "Ident"  p $ LitString i
showbLexemePrec p (Symbol s) = showbUnary "Symbol" p $ LitString s
#if MIN_VERSION_base(4,6,0)
showbLexemePrec p (Number n) = showbUnary "Number" p $ FromStringShow n
#else
showbLexemePrec p (Int i)    = showbUnary "Int" p i
showbLexemePrec p (Rat r)    = showbUnary "Rat" p r
#endif
showbLexemePrec _ EOF        = "EOF"
{-# INLINE showbLexemePrec #-}

#if MIN_VERSION_base(4,7,0)
-- | Convert a 'Number' to a 'Builder' with the given precedence.
showbNumberPrec :: Int -> Number -> Builder
showbNumberPrec p = showbPrec p . FromStringShow
{-# INLINE showbNumberPrec #-}
#endif

instance Show Lexeme where
    showbPrec = showbLexemePrec
    INLINE(showbPrec)

#if MIN_VERSION_base(4,7,0)
instance Show Number where
    showbPrec = showbNumberPrec
    INLINE(showbPrec)
#endif
