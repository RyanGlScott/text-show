{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      TextShow.Data.Char
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'TextShow' instances and monomorphic functions for 'Char' and 'String'.

/Since: 2/
-}
module TextShow.Data.Char (
      showbChar
    , showbLitChar
    , showbString
    , showbLitString
    , showbGeneralCategory
    , asciiTabB
    ) where

import           Data.Array (Array, (!), listArray)
import           Data.Char (GeneralCategory, isDigit, ord)
import           Data.Text.Lazy.Builder (Builder, singleton)

import           Prelude ()
import           Prelude.Compat

import           TextShow.Classes (TextShow(..))
import           TextShow.Data.Integral ()
import           TextShow.TH.Internal (deriveTextShow)

-- | /Since: 2/
$(deriveTextShow ''GeneralCategory)

-- | /Since: 2/
instance TextShow Char where
    showb = showbChar
    {-# INLINE showb #-}

    showbList = showbString
    {-# INLINE showbList #-}

-- | A table of ASCII control characters that needs to be escaped with a backslash.
--
-- /Since: 2/
asciiTabB :: Array Int Builder
asciiTabB = listArray (0, 32) ["NUL", "SOH", "STX", "ETX", "EOT", "ENQ", "ACK", "BEL",
                               "BS" , "HT" , "LF" , "VT" , "FF" , "CR" , "SO" , "SI" ,
                               "DLE", "DC1", "DC2", "DC3", "DC4", "NAK", "SYN", "ETB",
                               "CAN", "EM" , "SUB", "ESC", "FS" , "GS" , "RS" , "US" ,
                               "SP"]

-- | Convert a 'Char' to a 'Builder' (surrounded by single quotes).
--
-- /Since: 2/
showbChar :: Char -> Builder
showbChar '\'' = "'\\''"
showbChar c    = singleton '\'' <> showbLitChar c <> singleton '\''
{-# INLINE showbChar #-}

-- | Convert a 'Char' to a 'Builder' (without single quotes).
--
-- /Since: 2/
showbLitChar :: Char -> Builder
showbLitChar c | c > '\DEL' = singleton '\\' <> showb (ord c)
showbLitChar '\DEL'         = "\\DEL"
showbLitChar '\\'           = "\\\\"
showbLitChar c | c >= ' '   = singleton c
showbLitChar '\a'           = "\\a"
showbLitChar '\b'           = "\\b"
showbLitChar '\f'           = "\\f"
showbLitChar '\n'           = "\\n"
showbLitChar '\r'           = "\\r"
showbLitChar '\t'           = "\\t"
showbLitChar '\v'           = "\\v"
showbLitChar '\SO'          = "\\SO"
showbLitChar c              = singleton '\\' <> (asciiTabB ! ord c)

-- | Convert a 'String' to a 'Builder' (surrounded by double quotes).
--
-- /Since: 2/
showbString :: String -> Builder
showbString cs = singleton '"' <> showbLitString cs <> singleton '"'
{-# INLINE showbString #-}

-- | Convert a 'String' to a 'Builder' (without double quotes).
--
-- /Since: 2/
showbLitString :: String -> Builder
showbLitString []             = mempty
showbLitString ('\SO':'H':cs) = "\\SO\\&H" <> showbLitString cs
showbLitString ('"':cs)       = "\\\"" <> showbLitString cs
showbLitString (c:d:cs)
    | c > '\DEL' && isDigit d = singleton '\\' <> showb (ord c) <> "\\&"
                             <> singleton d    <> showbLitString cs
showbLitString (c:cs)         = showbLitChar c <> showbLitString cs

-- | Convert a 'GeneralCategory' to a 'Builder'.
--
-- /Since: 2/
showbGeneralCategory :: GeneralCategory -> Builder
showbGeneralCategory = showb
{-# INLINE showbGeneralCategory #-}
