{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Show.Text.Char
-- Copyright   :  (C) 2014 Ryan Scott
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  Experimental
-- Portability :  GHC
-- 
-- Utilities for showing 'Char's and 'String's. This code is adapted from
-- GHC.Show.
----------------------------------------------------------------------------
module Text.Show.Text.Char (showbLitChar, showbLitString) where

import Data.Array
import Data.Char
import Data.Monoid
import Data.Text.Lazy.Builder
import Data.Text.Lazy.Builder.Int

import Text.Show.Text.Util

-- | A table of ASCII control characters that needs to be escaped with a backslash.
asciiTabB :: Array Int Builder
asciiTabB = listArray (0, 32) ["NUL", "SOH", "STX", "ETX", "EOT", "ENQ", "ACK", "BEL",
                              "BS",  "HT",  "LF",  "VT",  "FF",  "CR",  "SO",  "SI",
                              "DLE", "DC1", "DC2", "DC3", "DC4", "NAK", "SYN", "ETB",
                              "CAN", "EM",  "SUB", "ESC", "FS",  "GS",  "RS",  "US",
                              "SP"]

-- | Constructs a 'Builder' with one character (without single quotes).
showbLitChar :: Char -> Builder
showbLitChar c | c > '\DEL' = s '\\' <> decimal (ord c)
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
showbLitChar '\SO'          = "\\S0"
showbLitChar c              = s '\\' <> (asciiTabB ! ord c)

-- | Constructs a 'Builder' with one 'String' (without double quotes).
showbLitString :: String -> Builder
showbLitString []                                  = mempty
showbLitString (c1:cs@(c2:_)) | shouldEscape c1 c2 = s c1 <> "\\&" <> showbLitString cs
  where
    shouldEscape :: Char -> Char -> Bool
    shouldEscape '\SO' 'H' = True
    shouldEscape a     b  = a > '\DEL' && isDigit b
showbLitString ('"':cs)                            = "\\\"" <> showbLitString cs
showbLitString (c:cs)                              = s c <> showbLitString cs