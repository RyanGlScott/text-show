{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
#if MIN_VERSION_text(0,9,0)
{-# LANGUAGE TemplateHaskell   #-}
#endif
{-# OPTIONS_GHC -fno-warn-deprecations #-} -- TODO: Remove this later
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      TextShow.Data.Text
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Monomorphic 'TextShow' functions for 'Text' types.

/Since: 2/
-}
module TextShow.Data.Text (
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

import           TextShow.Classes (TextShow(..))
import           TextShow.Data.Char (showbString)
import           TextShow.Data.Integral (showbHex)
import           TextShow.TH.Internal (deriveTextShow)

#if MIN_VERSION_text(1,0,0)
import           Data.Text.Encoding (Decoding(..))
import           Data.Text.Lazy.Builder (singleton)
import           GHC.Show (appPrec)
import           TextShow.Classes (showbParen)
import           TextShow.Data.ByteString (showbByteStringStrict)
#endif

#if MIN_VERSION_text(1,1,0)
import           Data.Text.Internal.Fusion.Size (Size)
#endif

#include "inline.h"

-- | Convert a strict 'TS.Text' to a 'Builder'.
-- 'showbText' should not be confused with @fromText@, as 'showbText' escapes
-- certain characters (such as double quotes).
--
-- /Since: 2/
showbText :: TS.Text -> Builder
showbText = showbString . TS.unpack
{-# INLINE showbText #-}

-- | Convert a lazy 'TL.Text' to a 'Builder'.
-- 'showbTextLazy' should not be confused with @fromTextLazy@, as 'showbTextLazy'
-- escapes certain characters (such as double quotes).
--
-- /Since: 2/
showbTextLazy :: TL.Text -> Builder
showbTextLazy = showbString . TL.unpack
{-# INLINE showbTextLazy #-}

-- | Show a 'Builder' as if it were a 'String' (i.e., escape certain characters,
-- such as double quotes).
--
-- /Since: 2/
showbBuilder :: Builder -> Builder
showbBuilder = showbTextLazy . toLazyText
{-# INLINE showbBuilder #-}

-- | Convert an 'I16' value to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbI16Prec :: Int -> I16 -> Builder
showbI16Prec = showbPrec
{-# INLINE showbI16Prec #-}

-- | Convert a 'UnicodeException' to a 'Builder'.
--
-- /Since: 2/
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
-- /Since: 2/
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
-- /Since: 2/
showbSizePrec :: Int -> Size -> Builder
showbSizePrec = showbPrec
{-# INLINE showbSizePrec #-}
#endif

instance TextShow TS.Text where
    showb = showbText
    INLINE_INST_FUN(showb)

instance TextShow TL.Text where
    showb = showbTextLazy
    INLINE_INST_FUN(showb)

instance TextShow Builder where
    showb = showbBuilder
    INLINE_INST_FUN(showb)

$(deriveTextShow ''I16)

instance TextShow UnicodeException where
    showb = showbUnicodeException
    INLINE_INST_FUN(showb)

#if MIN_VERSION_text(1,0,0)
instance TextShow Decoding where
    showbPrec = showbDecodingPrec
    INLINE_INST_FUN(showbPrec)
#endif

#if MIN_VERSION_text(1,1,0)
$(deriveTextShow ''Size)
#endif
