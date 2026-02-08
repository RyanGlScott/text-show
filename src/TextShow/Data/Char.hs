{-# LANGUAGE MagicHash         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -Wno-orphans #-}
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
import           Data.Text.Builder.Linear (Builder, fromAddr, fromChar)

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
asciiTabB =
  listArray
    (0, 32)
    [fromAddr "NUL"#, fromAddr "SOH"#, fromAddr "STX"#, fromAddr "ETX"#,
     fromAddr "EOT"#, fromAddr "ENQ"#, fromAddr "ACK"#, fromAddr "BEL"#,
     fromAddr "BS"# , fromAddr "HT"# , fromAddr "LF"# , fromAddr "VT"# ,
     fromAddr "FF"# , fromAddr "CR"# , fromAddr "SO"# , fromAddr "SI"# ,
     fromAddr "DLE"#, fromAddr "DC1"#, fromAddr "DC2"#, fromAddr "DC3"#,
     fromAddr "DC4"#, fromAddr "NAK"#, fromAddr "SYN"#, fromAddr "ETB"#,
     fromAddr "CAN"#, fromAddr "EM"# , fromAddr "SUB"#, fromAddr "ESC"#,
     fromAddr "FS"# , fromAddr "GS"# , fromAddr "RS"# , fromAddr "US"# ,
     fromAddr "SP"#]

-- | Convert a 'Char' to a 'Builder' (surrounded by single quotes).
--
-- /Since: 2/
showbChar :: Char -> Builder
showbChar '\'' = "'\\''"
showbChar c    = fromChar '\'' <> showbLitChar c <> fromChar '\''
{-# INLINE showbChar #-}

-- | Convert a 'Char' to a 'Builder' (without single quotes).
--
-- /Since: 2/
showbLitChar :: Char -> Builder
showbLitChar c | c > '\DEL' = fromChar '\\' <> showb (ord c)
showbLitChar '\DEL'         = "\\DEL"
showbLitChar '\\'           = "\\\\"
showbLitChar c | c >= ' '   = fromChar c
showbLitChar '\a'           = fromAddr "\\a"#
showbLitChar '\b'           = fromAddr "\\b"#
showbLitChar '\f'           = fromAddr "\\f"#
showbLitChar '\n'           = fromAddr "\\n"#
showbLitChar '\r'           = fromAddr "\\r"#
showbLitChar '\t'           = fromAddr "\\t"#
showbLitChar '\v'           = fromAddr "\\v"#
showbLitChar '\SO'          = fromAddr "\\SO"#
showbLitChar c              = fromChar '\\' <> (asciiTabB ! ord c)

-- | Convert a 'String' to a 'Builder' (surrounded by double quotes).
--
-- /Since: 2/
showbString :: String -> Builder
showbString cs = fromChar '"' <> showbLitString cs <> fromChar '"'
{-# INLINE showbString #-}

-- | Convert a 'String' to a 'Builder' (without double quotes).
--
-- /Since: 2/
showbLitString :: String -> Builder
showbLitString []             = mempty
showbLitString ('\SO':'H':cs) = fromAddr "\\SO\\&H"# <> showbLitString cs
showbLitString ('"':cs)       = "\\\"" <> showbLitString cs
showbLitString (c:d:cs)
    | c > '\DEL' && isDigit d = fromChar '\\' <> showb (ord c) <> fromAddr "\\&"#
                             <> fromChar d    <> showbLitString cs
showbLitString (c:cs)         = showbLitChar c <> showbLitString cs

-- | Convert a 'GeneralCategory' to a 'Builder'.
--
-- /Since: 2/
showbGeneralCategory :: GeneralCategory -> Builder
showbGeneralCategory = showb
{-# INLINE showbGeneralCategory #-}
