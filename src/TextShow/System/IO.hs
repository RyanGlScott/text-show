{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      TextShow.System.IO
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Monomorphic 'TextShow' function for 'IO'-related data types.

/Since: 2/
-}
module TextShow.System.IO (
      showbHandle
    , showbIOMode
    , showbBufferModePrec
    , showbHandlePosn
    , showbSeekMode
    , showbTextEncoding
#if MIN_VERSION_base(4,4,0)
    , showbCodingProgress
    , showbCodingFailureMode
#endif
    , showbNewline
    , showbNewlineModePrec
    ) where

import Data.Monoid.Compat ((<>))
import Data.Text.Lazy.Builder (Builder, fromString, singleton)

import GHC.IO.Encoding.Types (TextEncoding(textEncodingName))
#if MIN_VERSION_base(4,4,0)
import GHC.IO.Encoding.Failure (CodingFailureMode)
import GHC.IO.Encoding.Types (CodingProgress)
#endif
import GHC.IO.Handle (HandlePosn(..))
import GHC.IO.Handle.Types (Handle(..))

import System.IO (BufferMode, IOMode, Newline, NewlineMode, SeekMode)

import TextShow.Classes (TextShow(..))
import TextShow.Data.Integral (showbIntegerPrec)
import TextShow.Data.Maybe ()
import TextShow.TH.Internal (deriveTextShow)

#include "inline.h"

-- | Convert a 'Handle' to a 'Builder'.
--
-- /Since: 2/
showbHandle :: Handle -> Builder
showbHandle (FileHandle   file _)   = showbHandleFilePath file
showbHandle (DuplexHandle file _ _) = showbHandleFilePath file
{-# INLINE showbHandle #-}

-- | Convert a 'Handle`'s 'FilePath' to a 'Builder'.
showbHandleFilePath :: FilePath -> Builder
showbHandleFilePath file = "{handle: " <> fromString file <> singleton '}'
{-# INLINE showbHandleFilePath #-}

-- | Convert an 'IOMode' to a 'Builder'.
--
-- /Since: 2/
showbIOMode :: IOMode -> Builder
showbIOMode = showb
{-# INLINE showbIOMode #-}

-- | Convert a 'BufferMode' to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbBufferModePrec :: Int -> BufferMode -> Builder
showbBufferModePrec = showbPrec
{-# INLINE showbBufferModePrec #-}

-- | Convert a 'HandlePosn' to a 'Builder'.
--
-- /Since: 2/
showbHandlePosn :: HandlePosn -> Builder
showbHandlePosn (HandlePosn h pos)
    = showbHandle h <> " at position " <> showbIntegerPrec 0 pos
{-# INLINE showbHandlePosn #-}

-- | Convert a 'SeekMode' to a 'Builder'.
--
-- /Since: 2/
showbSeekMode :: SeekMode -> Builder
showbSeekMode = showb
{-# INLINE showbSeekMode #-}

-- | Convert a 'TextEncoding' to a 'Builder'.
--
-- /Since: 2/
showbTextEncoding :: TextEncoding -> Builder
showbTextEncoding = fromString . textEncodingName
{-# INLINE showbTextEncoding #-}

#if MIN_VERSION_base(4,4,0)
-- | Convert a 'CodingProgress' to a 'Builder'.
-- This function is only available with @base-4.4.0.0@ or later.
--
-- /Since: 2/
showbCodingProgress :: CodingProgress -> Builder
showbCodingProgress = showb
{-# INLINE showbCodingProgress #-}

-- | Convert a 'CodingFailureMode' value to a 'Builder'.
-- This function is only available with @base-4.4.0.0@ or later.
--
-- /Since: 2/
showbCodingFailureMode :: CodingFailureMode -> Builder
showbCodingFailureMode = showb
{-# INLINE showbCodingFailureMode #-}
#endif

-- | Convert a 'Newline' to a 'Builder'.
--
-- /Since: 2/
showbNewline :: Newline -> Builder
showbNewline = showb
{-# INLINE showbNewline #-}

-- | Convert a 'NewlineMode' to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbNewlineModePrec :: Int -> NewlineMode -> Builder
showbNewlineModePrec = showbPrec
{-# INLINE showbNewlineModePrec #-}

instance TextShow Handle where
    showb = showbHandle
    INLINE_INST_FUN(showb)

$(deriveTextShow ''IOMode)
$(deriveTextShow ''BufferMode)

instance TextShow HandlePosn where
    showb = showbHandlePosn
    INLINE_INST_FUN(showb)

$(deriveTextShow ''SeekMode)

instance TextShow TextEncoding where
    showb = showbTextEncoding
    INLINE_INST_FUN(showb)

#if MIN_VERSION_base(4,4,0)
$(deriveTextShow ''CodingProgress)
$(deriveTextShow ''CodingFailureMode)
#endif

$(deriveTextShow ''Newline)
$(deriveTextShow ''NewlineMode)
