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

import           Data.Text.Lazy.Builder (Builder, fromString)

import           GHC.Show (appPrec, appPrec1)

import           Prelude hiding (Show)

import           Text.Read.Lex (Lexeme(..))
#if MIN_VERSION_base(4,7,0)
import           Text.Read.Lex (Number)
#endif
import           Text.Show.Text.Classes (Show(showbPrec), showbParen)
import           Text.Show.Text.Data.Char (showbLitChar)
#if MIN_VERSION_base(4,6,0)
import qualified Text.Show as S (showsPrec)
#else
import           Text.Show.Text.Data.Integral (showbIntegerPrec)
import           Text.Show.Text.Data.Ratio (showbRatioPrec)
#endif
import           Text.Show.Text.Utils ((<>))

-- | Convert a 'Lexeme' to a 'Builder' with the given precedence.
showbLexemePrec :: Int -> Lexeme -> Builder
showbLexemePrec p (Char c)   = showbParen (p > appPrec) $ "Char "   <> showbLitChar c
showbLexemePrec p (String s) = showbParen (p > appPrec) $ "String " <> fromString s
showbLexemePrec p (Punc pun) = showbParen (p > appPrec) $ "Punc "   <> fromString pun
showbLexemePrec p (Ident i)  = showbParen (p > appPrec) $ "Ident "  <> fromString i
showbLexemePrec p (Symbol s) = showbParen (p > appPrec) $ "Symbol " <> fromString s
#if MIN_VERSION_base(4,6,0)
showbLexemePrec p (Number n) = showbParen (p > appPrec) $ "Number " <> fromString (S.showsPrec appPrec1 n "")
#else
showbLexemePrec p (Int i)    = showbParen (p > appPrec) $ "Int "    <> showbIntegerPrec appPrec1 i
showbLexemePrec p (Rat r)    = showbParen (p > appPrec) $ "Rat "    <> showbRatioPrec appPrec1 r
#endif
showbLexemePrec _ EOF        = "EOF"
{-# INLINE showbLexemePrec #-}

#if MIN_VERSION_base(4,7,0)
-- | Convert a 'Number' to a 'Builder' with the given precedence.
showbNumberPrec :: Int -> Number -> Builder
showbNumberPrec p n = fromString $ S.showsPrec p n ""
{-# INLINE showbNumberPrec #-}
#endif

instance Show Lexeme where
    showbPrec = showbLexemePrec
    {-# INLINE showbPrec #-}

#if MIN_VERSION_base(4,7,0)
instance Show Number where
    showbPrec = showbNumberPrec
    {-# INLINE showbPrec #-}
#endif
