{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Text.Show.Text.Char where

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

-- | Constructs a 'Builder' with one character (without single quotes).
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

showbLitString :: String -> Builder
showbLitString []             = mempty
showbLitString ('\SO':'H':cs) = "\\SO\\&H" <> showbLitString cs
showbLitString ('"':cs)       = "\\\"" <> showbLitString cs
showbLitString (c:d:cs)
    | c > '\DEL' && isDigit d = s '\\' <> build (ord c) <> "\\&" <> s d <> showbLitString cs
showbLitString (c:cs)         = showbLitChar c <> showbLitString cs
{-# INLINE showbLitString #-}

instance Show Char where
    showb '\'' = "'\\''"
    showb c    = s '\'' <> showbLitChar c <> s '\''
    {-# INLINE showb #-}
    
    showbList cs = s '"' <> showbLitString cs <> s '"'
    {-# INLINE showbList #-}