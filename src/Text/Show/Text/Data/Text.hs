{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
#if MIN_VERSION_text(0,9,0)
{-# LANGUAGE TemplateHaskell   #-}
#endif
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.Data.Text
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' functions for 'Text' types.

/Since: 0.3/
-}
module Text.Show.Text.Data.Text (
      showbText
    , showbTextLazy
    , showbBuilder
    , showbI16Prec
    , showbUnicodeException
#if MIN_VERSION_text(1,0,0)
    , showbDecodingPrec
#endif
#if MIN_VERSION_text(1,1,0)
    , showbSizePrec
#endif
    ) where

import           Data.Monoid.Compat ((<>))
import qualified Data.Text as TS
import           Data.Text.Encoding.Error (UnicodeException(..))
import           Data.Text.Foreign (I16)
import qualified Data.Text.Lazy as TL
import           Data.Text.Lazy.Builder (Builder, fromString, toLazyText)
import           Prelude hiding (Show)

import           Text.Show.Text.Classes (Show(showb, showbPrec))
import           Text.Show.Text.Data.Char (showbString)
import           Text.Show.Text.Data.Integral (showbHex)
import           Text.Show.Text.TH.Internal (deriveShowPragmas, defaultInlineShowbPrec)

#if MIN_VERSION_text(1,0,0)
import           Data.Text.Encoding (Decoding(..))
import           Data.Text.Lazy.Builder (singleton)
import           GHC.Show (appPrec)
import           Text.Show.Text.Classes (showbParen)
import           Text.Show.Text.Data.ByteString (showbByteStringStrict)
#endif

#if MIN_VERSION_text(1,1,0)
import           Data.Text.Internal.Fusion.Size (Size)
import           Text.Show.Text.TH.Internal (deriveShow)
#endif

#include "inline.h"

-- | Convert a strict 'TS.Text' to a 'Builder'.
-- 'showbText' should not be confused with @fromText@, as 'showbText' escapes
-- certain characters (such as double quotes).
-- 
-- /Since: 0.5/
showbText :: TS.Text -> Builder
showbText = showbString . TS.unpack
{-# INLINE showbText #-}

-- | Convert a lazy 'TL.Text' to a 'Builder'.
-- 'showbTextLazy' should not be confused with @fromTextLazy@, as 'showbTextLazy'
-- escapes certain characters (such as double quotes).
-- 
-- /Since: 0.3/
showbTextLazy :: TL.Text -> Builder
showbTextLazy = showbString . TL.unpack
{-# INLINE showbTextLazy #-}

-- | Show a 'Builder' as if it were a 'String' (i.e., escape certain characters,
-- such as double quotes).
-- 
-- /Since: 0.5/
showbBuilder :: Builder -> Builder
showbBuilder = showbTextLazy . toLazyText
{-# INLINE showbBuilder #-}

-- | Convert an 'I16' value to a 'Builder' with the given precedence.
-- 
-- /Since: 0.8/
showbI16Prec :: Int -> I16 -> Builder
showbI16Prec = showbPrec
{-# INLINE showbI16Prec #-}

-- | Convert a 'UnicodeException' to a 'Builder'.
-- 
-- /Since: 0.8/
showbUnicodeException :: UnicodeException -> Builder
showbUnicodeException (DecodeError desc (Just w))
    = "Cannot decode byte '\\x" <> showbHex w <> "': " <> fromString desc
showbUnicodeException (DecodeError desc Nothing)
    = "Cannot decode input: " <> fromString desc
showbUnicodeException (EncodeError desc (Just c))
    = "Cannot encode character '\\x" <> showbHex (fromEnum c) <> "': " <> fromString desc
showbUnicodeException (EncodeError desc Nothing)
    = "Cannot encode input: " <> fromString desc

#if MIN_VERSION_text(1,0,0)
-- | Convert a 'Decoding' value to a 'Builder' with the given precedence.
-- This function is only available with @text-1.0.0.0@ or later.
-- 
-- /Since: 0.8/
showbDecodingPrec :: Int -> Decoding -> Builder
showbDecodingPrec p (Some t bs _) = showbParen (p > appPrec) $
    fromString "Some " <> showbText t <>
    singleton ' ' <> showbByteStringStrict bs <>
    fromString " _"
{-# INLINE showbDecodingPrec #-}
#endif

#if MIN_VERSION_text(1,1,0)
-- | Convert a 'Size' value to a 'Builder' with the given precedence.
-- This function is only available with @text-1.1.0.0@ or later.
-- 
-- /Since: 0.8/
showbSizePrec :: Int -> Size -> Builder
showbSizePrec = showbPrec
{-# INLINE showbSizePrec #-}
#endif

instance Show TS.Text where
    showb = showbText
    INLINE_INST_FUN(showb)

instance Show TL.Text where
    showb = showbTextLazy
    INLINE_INST_FUN(showb)

instance Show Builder where
    showb = showbBuilder
    INLINE_INST_FUN(showb)

$(deriveShowPragmas defaultInlineShowbPrec ''I16)

instance Show UnicodeException where
    showb = showbUnicodeException
    INLINE_INST_FUN(showb)

#if MIN_VERSION_text(1,0,0)
instance Show Decoding where
    showbPrec = showbDecodingPrec
    INLINE_INST_FUN(showbPrec)
#endif

#if MIN_VERSION_text(1,1,0)
$(deriveShow ''Size)
#endif
