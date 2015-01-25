{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleInstances,
             GeneralizedNewtypeDeriving, OverloadedStrings, TemplateHaskell #-}
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
import           Data.Functor ((<$>))
import           Data.Ix (Ix)
#if !(MIN_VERSION_base(4,8,0))
import           Data.Monoid (Monoid(mempty))
#endif
import           Data.Semigroup (Semigroup)
import           Data.String (IsString(..))
import           Data.Text.Lazy.Builder (Builder)

import           Foreign.Storable (Storable)

#if __GLASGOW_HASKELL__ >= 708
import           GHC.Exts (IsList(Item, fromList, toList))
#endif
#if MIN_VERSION_base(4,4,0)
import           GHC.Generics (Generic)
#endif
import           GHC.Show (showLitChar)
#if MIN_VERSION_base(4,4,0)
import           GHC.Show (showLitString)
#endif

import           Prelude hiding (Show)

import qualified Text.ParserCombinators.ReadP as ReadP (get)
import           Text.ParserCombinators.ReadP (many)
import qualified Text.ParserCombinators.ReadPrec as ReadPrec (get)
import           Text.ParserCombinators.ReadPrec (ReadPrec, lift)
import           Text.Printf (IsChar, PrintfArg, PrintfType)
import           Text.Read (Read(..), readListPrecDefault)
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
           , IsChar
           , Ix
           , Ord
           , PrintfArg
           , Storable
           , Typeable
           )

instance IsString [LitChar] where
    fromString = map LitChar

instance Read LitChar where
    readPrec = LitChar <$> ReadPrec.get
    INLINE_INST_FUN(readPrec)
    
    readListPrec = (fmap . map) LitChar (readListPrec :: ReadPrec [Char])
    INLINE_INST_FUN(readListPrec)
    
    readList =
        (fmap . map . mapFst . map) LitChar (readList :: ReadS [Char])
      where
        mapFst :: (a -> b) -> (a, c) -> (b, c)
        mapFst f (x, y) = (f x, y)
    INLINE_INST_FUN(readList)

instance Show LitChar where
    showb = showbLitChar . getLitChar
    INLINE_INST_FUN(showb)
    
    showbList = showbString . map getLitChar
    INLINE_INST_FUN(showbList)

instance S.Show LitChar where
    showsPrec _ = showLitChar . getLitChar
    INLINE_INST_FUN(showsPrec)
    
    showList = showList . map getLitChar
    INLINE_INST_FUN(showList)

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
           , Semigroup
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

instance Read LitString where
    readPrec = LitString <$> (lift $ many ReadP.get)
    INLINE_INST_FUN(readPrec)
    
    readListPrec = readListPrecDefault
    INLINE_INST_FUN(readListPrec)

instance Show LitString where
    showb = showbLitString . getLitString
    INLINE_INST_FUN(showb)

instance S.Show LitString where
    showsPrec _ = showLitString . getLitString
#if MIN_VERSION_base(4,4,0)
    INLINE_INST_FUN(showsPrec)
#else
      where
        -- Taken directly from @GHC.Show@
        showLitString :: String -> ShowS
        -- Same as 'showLitChar', but for strings
        -- It converts the string to a string using Haskell escape conventions
        -- for non-printable characters. Does not add double-quotes around the
        -- whole thing; the caller should do that.
        -- The main difference from showLitChar (apart from the fact that the
        -- argument is a string not a list) is that we must escape double-quotes 
        showLitString []         str = str
        showLitString ('"' : cs) str = showString "\\\"" (showLitString cs str)
        showLitString (c   : cs) str = showLitChar c (showLitString cs str)
           -- Making 's' an explicit parameter makes it clear to GHC that
           -- showLitString has arity 2, which avoids it allocating an extra lambda
           -- The sticking point is the recursive call to (showLitString cs), which
           -- it can't figure out would be ok with arity 2.
#endif