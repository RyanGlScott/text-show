{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.System.IO
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' function for 'IO'-related data types.

/Since: 0.3/
-}
module Text.Show.Text.System.IO (
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
import Data.Text.Lazy.Builder (Builder, fromString)

import GHC.IO.Encoding.Types (TextEncoding(textEncodingName))
#if MIN_VERSION_base(4,4,0)
import GHC.IO.Encoding.Failure (CodingFailureMode)
import GHC.IO.Encoding.Types (CodingProgress)
#endif
import GHC.IO.Handle (HandlePosn(..))
import GHC.IO.Handle.Types (Handle(..))

import Prelude hiding (Show)

import System.IO (BufferMode, IOMode, Newline, NewlineMode, SeekMode)

import Text.Show.Text.Classes (Show(showb, showbPrec))
import Text.Show.Text.Data.Integral (showbIntegerPrec)
import Text.Show.Text.Data.Maybe ()
import Text.Show.Text.TH.Internal (deriveShow, deriveShowPragmas,
                                   inlineShowb, inlineShowbPrec)
import Text.Show.Text.Utils (s)

#include "inline.h"

-- | Convert a 'Handle' to a 'Builder'.
-- 
-- /Since: 0.3/
showbHandle :: Handle -> Builder
showbHandle (FileHandle   file _)   = showbHandleFilePath file
showbHandle (DuplexHandle file _ _) = showbHandleFilePath file
{-# INLINE showbHandle #-}

-- | Convert a 'Handle`'s 'FilePath' to a 'Builder'.
showbHandleFilePath :: FilePath -> Builder
showbHandleFilePath file = "{handle: " <> fromString file <> s '}'
{-# INLINE showbHandleFilePath #-}

-- | Convert an 'IOMode' to a 'Builder'.
-- 
-- /Since: 0.3/
showbIOMode :: IOMode -> Builder
showbIOMode = showb
{-# INLINE showbIOMode #-}

-- | Convert a 'BufferMode' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.3/
showbBufferModePrec :: Int -> BufferMode -> Builder
showbBufferModePrec = showbPrec
{-# INLINE showbBufferModePrec #-}

-- | Convert a 'HandlePosn' to a 'Builder'.
-- 
-- /Since: 0.3/
showbHandlePosn :: HandlePosn -> Builder
showbHandlePosn (HandlePosn h pos)
    = showbHandle h <> " at position " <> showbIntegerPrec 0 pos
{-# INLINE showbHandlePosn #-}

-- | Convert a 'SeekMode' to a 'Builder'.
-- 
-- /Since: 0.3/
showbSeekMode :: SeekMode -> Builder
showbSeekMode = showb
{-# INLINE showbSeekMode #-}

-- | Convert a 'TextEncoding' to a 'Builder'.
-- 
-- /Since: 0.3/
showbTextEncoding :: TextEncoding -> Builder
showbTextEncoding = fromString . textEncodingName
{-# INLINE showbTextEncoding #-}

#if MIN_VERSION_base(4,4,0)
-- | Convert a 'CodingProgress' to a 'Builder'.
-- This function is only available with @base-4.4.0.0@ or later.
-- 
-- /Since: 0.3/
showbCodingProgress :: CodingProgress -> Builder
showbCodingProgress = showb
{-# INLINE showbCodingProgress #-}

-- | Convert a 'CodingFailureMode' value to a 'Builder'.
-- This function is only available with @base-4.4.0.0@ or later.
-- 
-- /Since: 0.3/
showbCodingFailureMode :: CodingFailureMode -> Builder
showbCodingFailureMode = showb
{-# INLINE showbCodingFailureMode #-}
#endif

-- | Convert a 'Newline' to a 'Builder'.
-- 
-- /Since: 0.3/
showbNewline :: Newline -> Builder
showbNewline = showb
{-# INLINE showbNewline #-}

-- | Convert a 'NewlineMode' to a 'Builder' with the given precedence.
-- 
-- /Since: 0.3/
showbNewlineModePrec :: Int -> NewlineMode -> Builder
showbNewlineModePrec = showbPrec
{-# INLINE showbNewlineModePrec #-}

instance Show Handle where
    showb = showbHandle
    INLINE_INST_FUN(showb)

$(deriveShowPragmas inlineShowb     ''IOMode)
$(deriveShow                        ''BufferMode)

instance Show HandlePosn where
    showb = showbHandlePosn
    INLINE_INST_FUN(showb)

$(deriveShowPragmas inlineShowb     ''SeekMode)

instance Show TextEncoding where
    showb = showbTextEncoding
    INLINE_INST_FUN(showb)

#if MIN_VERSION_base(4,4,0)
$(deriveShowPragmas inlineShowb     ''CodingProgress)
$(deriveShowPragmas inlineShowb     ''CodingFailureMode)
#endif

$(deriveShowPragmas inlineShowb     ''Newline)
$(deriveShowPragmas inlineShowbPrec ''NewlineMode)
