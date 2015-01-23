{-# LANGUAGE CPP, DeriveDataTypeable, GeneralizedNewtypeDeriving,
             OverloadedStrings, TemplateHaskell #-}
#if MIN_VERSION_base(4,4,0)
{-# LANGUAGE DeriveGeneric #-}
#endif
#if __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE TypeFamilies #-}
#endif
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.Data.Char
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' functions for 'Char' and 'String'.

/Since: 0.3/
-}
module Text.Show.Text.Data.Char (
      showbChar
    , showbLitChar
    , showbString
    , showbLitString
    , showbGeneralCategory
    , asciiTabB
    , LitChar(..)
    , LitString(..)
    ) where

import           Data.Array (Array, (!), listArray)
import           Data.Char (GeneralCategory, isDigit, ord)
import           Data.Data (Data, Typeable)
import           Data.Ix (Ix)
#if !(MIN_VERSION_base(4,8,0))
import           Data.Monoid (Monoid(mempty))
#endif
import           Data.String (IsString)
import           Data.Text.Lazy.Builder (Builder)

import           Foreign.Storable (Storable)

#if __GLASGOW_HASKELL__ >= 708
import           GHC.Exts (IsList(Item, fromList, toList))
#endif
#if MIN_VERSION_base(4,4,0)
import           GHC.Generics (Generic)
#endif

import           Prelude hiding (Show)

import           Text.Printf (PrintfArg, PrintfType)
import qualified Text.Show as S (Show)
import           Text.Show.Text.Classes (Show(..))
import           Text.Show.Text.Data.Integral (showbIntPrec)
import           Text.Show.Text.TH.Internal (deriveShow)
import           Text.Show.Text.Utils ((<>), s)

#include "inline.h"

-- | A table of ASCII control characters that needs to be escaped with a backslash.
-- 
-- /Since: 0.5/
asciiTabB :: Array Int Builder
asciiTabB = listArray (0, 32) ["NUL", "SOH", "STX", "ETX", "EOT", "ENQ", "ACK", "BEL",
                              "BS",  "HT",  "LF",  "VT",  "FF",  "CR",  "SO",  "SI",
                              "DLE", "DC1", "DC2", "DC3", "DC4", "NAK", "SYN", "ETB",
                              "CAN", "EM",  "SUB", "ESC", "FS",  "GS",  "RS",  "US",
                              "SP"]

-- | Convert a 'Char' to a 'Builder' (surrounded by single quotes).
-- 
-- /Since: 0.3/
showbChar :: Char -> Builder
showbChar '\'' = "'\\''"
showbChar c    = s '\'' <> showbLitChar c <> s '\''
{-# INLINE showbChar #-}

-- | Convert a 'Char' to a 'Builder' (without single quotes).
-- 
-- /Since: 0.3/
showbLitChar :: Char -> Builder
showbLitChar c | c > '\DEL' = s '\\' <> showbIntPrec 0 (ord c)
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

-- | Convert a 'String' to a 'Builder' (surrounded by double quotes).
-- 
-- /Since: 0.3/
showbString :: String -> Builder
showbString cs = s '"' <> showbLitString cs <> s '"'
{-# INLINE showbString #-}

-- | Convert a 'String' to a 'Builder' (without double quotes).
-- 
-- /Since: 0.3/
showbLitString :: String -> Builder
showbLitString []             = mempty
showbLitString ('\SO':'H':cs) = "\\SO\\&H" <> showbLitString cs
showbLitString ('"':cs)       = "\\\"" <> showbLitString cs
showbLitString (c:d:cs)
    | c > '\DEL' && isDigit d = s '\\' <> showbIntPrec 0 (ord c) <> "\\&" <> s d <> showbLitString cs
showbLitString (c:cs)         = showbLitChar c <> showbLitString cs

-- | Convert a 'GeneralCategory' to a 'Builder'.
-- 
-- /Since: 0.3/
showbGeneralCategory :: GeneralCategory -> Builder
showbGeneralCategory = showb
{-# INLINE showbGeneralCategory #-}

instance Show Char where
    showb = showbChar
    INLINE_INST_FUN(showb)
    
    showbList = showbString
    INLINE_INST_FUN(showbList)

$(deriveShow ''GeneralCategory)

-- | The @Text@ 'T.Show' instance for 'LitChar' is like that of a regular 'Char',
-- except it is not escaped by single quotes. That is,
-- 
-- @
-- showb ('LitChar' c) = 'showbLitChar' c
-- @
-- 
-- /Since: 0.5/
newtype LitChar = LitChar { getLitChar :: Char }
  deriving ( Bounded
           , Data
           , Enum
           , Eq
#if MIN_VERSION_base(4,4,0)
           , Generic
#endif
           , Ix
           , Ord
           , PrintfArg
           , Read
           , S.Show
           , Storable
           , Typeable
           )

instance Show LitChar where
    showb = showbLitChar . getLitChar
    INLINE_INST_FUN(showb)

-- | The @Text@ 'T.Show' instance for 'LitString' is like that of a regular
-- 'String', except it is not escaped by double quotes. That is,
-- 
-- @
-- showb ('LitString' s) = 'showbLitString' s
-- @
-- 
-- /Since: 0.5/
newtype LitString = LitString { getLitString :: String }
  deriving ( Data
           , Eq
#if MIN_VERSION_base(4,4,0)
           , Generic
#endif
           , IsString
           , Monoid
           , Ord
           , PrintfArg
           , PrintfType
           , Read
           , S.Show
           , Typeable
           )

#if __GLASGOW_HASKELL__ >= 708
instance IsList LitString where
    type Item LitString = Char
    fromList = LitString
    {-# INLINE fromList #-}
    toList = getLitString
    {-# INLINE toList #-}
#endif

instance Show LitString where
    showb = showbLitString . getLitString
    INLINE_INST_FUN(showb)
