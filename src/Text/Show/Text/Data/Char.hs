{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Show.Text.Data.Char
-- Copyright   :  (C) 2014 Ryan Scott
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  Experimental
-- Portability :  GHC
-- 
-- Monomorphic 'Show' functions for 'Char' and 'String'.
----------------------------------------------------------------------------
module Text.Show.Text.Data.Char (
      showbChar
    , showbLitChar
    , showbString
    , showbLitString
    ) where

import Data.Array (Array, (!), listArray)
import Data.Char (isDigit, ord)
import Data.Monoid (mempty, (<>))
import Data.Text.Buildable (build)
import Data.Text.Lazy.Builder (Builder)

import Prelude hiding (Show)

import Text.Show.Text.Class (Show(..))
import Text.Show.Text.Functions (s)

-- | A table of ASCII control characters that needs to be escaped with a backslash.
asciiTabB :: Array Int Builder
asciiTabB = listArray (0, 32) ["NUL", "SOH", "STX", "ETX", "EOT", "ENQ", "ACK", "BEL",
                              "BS",  "HT",  "LF",  "VT",  "FF",  "CR",  "SO",  "SI",
                              "DLE", "DC1", "DC2", "DC3", "DC4", "NAK", "SYN", "ETB",
                              "CAN", "EM",  "SUB", "ESC", "FS",  "GS",  "RS",  "US",
                              "SP"]

-- | Convert a 'Char' to a 'Builder' (surrounded by single quotes).
showbChar :: Char -> Builder
showbChar '\'' = "'\\''"
showbChar c    = s '\'' <> showbLitChar c <> s '\''
{-# INLINE showbChar #-}

-- | Convert a 'Char' to a 'Builder' (without single quotes).
showbLitChar :: Char -> Builder
showbLitChar c | c > '\DEL' = s '\\' <> build (ord c)
showbLitChar '\DEL'         = "\\DEL"
showbLitChar '\\'           = "\\\\"
showbLitChar c | c >= ' '   = s c
showbLitChar '\a'           = "\\a"
showbLitChar '\b'           = "\\b"
showbLitChar '\f'           = "\\f"
showbLitChar '\n'           = "\\n"
showbLitChar '\r'           = "\\r"
showbLitChar '\t'           = "\\t"
showbLitChar '\v'           = "\\v"
showbLitChar '\SO'          = "\\SO"
showbLitChar c              = s '\\' <> (asciiTabB ! ord c)
{-# INLINE showbLitChar #-}

-- | Convert a 'String' to a 'Builder' (surrounded by double quotes).
showbString :: String -> Builder
showbString cs = s '"' <> showbLitString cs <> s '"'
{-# INLINE showbString #-}

-- | Convert a 'String' to a 'Builder' (without double quotes).
showbLitString :: String -> Builder
showbLitString []             = mempty
showbLitString ('\SO':'H':cs) = "\\SO\\&H" <> showbLitString cs
showbLitString ('"':cs)       = "\\\"" <> showbLitString cs
showbLitString (c:d:cs)
    | c > '\DEL' && isDigit d = s '\\' <> build (ord c) <> "\\&" <> s d <> showbLitString cs
showbLitString (c:cs)         = showbLitChar c <> showbLitString cs
{-# INLINE showbLitString #-}

instance Show Char where
    showb = showbChar
    {-# INLINE showb #-}
    
    showbList = showbString
    {-# INLINE showbList #-}