{-# LANGUAGE CPP, OverloadedStrings, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.System.IO
Copyright:   (C) 2014 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' function for 'IO'-related data types.
-}
module Text.Show.Text.System.IO (
      showbHandle
    , showbIOMode
    , showbBufferModePrec
    , showbHandlePosn
    , showbSeekMode
#if MIN_VERSION_base(4,3,0)
    , showbTextEncoding
#endif
#if MIN_VERSION_base(4,4,0)
    , showbCodingProgress
    , showbCodingFailureMode
#endif
    , showbNewline
    , showbNewlineModePrec
    ) where

import Data.Text.Lazy.Builder (Builder, fromString)

#if MIN_VERSION_base(4,3,0)
import GHC.IO.Encoding.Types (TextEncoding(textEncodingName))
#endif
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
import Text.Show.Text.TH.Internal (deriveShowPragmas, defaultInlineShowb,
                                   defaultInlineShowbPrec)
import Text.Show.Text.Utils ((<>), s)

-- | Convert a 'Handle' to a 'Builder'.
showbHandle :: Handle -> Builder
showbHandle (FileHandle   file _)   = showbHandleFilePath file
showbHandle (DuplexHandle file _ _) = showbHandleFilePath file
{-# INLINE showbHandle #-}

-- | Convert a 'Handle`'s 'FilePath' to a 'Builder'.
showbHandleFilePath :: FilePath -> Builder
showbHandleFilePath file = "{handle: " <> fromString file <> s '}'
{-# INLINE showbHandleFilePath #-}

-- | Convert an 'IOMode' to a 'Builder'.
showbIOMode :: IOMode -> Builder
showbIOMode = showb
{-# INLINE showbIOMode #-}

-- | Convert a 'BufferMode' to a 'Builder' with the given precedence.
showbBufferModePrec :: Int -> BufferMode -> Builder
showbBufferModePrec = showbPrec
{-# INLINE showbBufferModePrec #-}

-- | Convert a 'HandlePosn' to a 'Builder'.
showbHandlePosn :: HandlePosn -> Builder
showbHandlePosn (HandlePosn h pos)
    = showbHandle h <> " at position " <> showbIntegerPrec 0 pos
{-# INLINE showbHandlePosn #-}

-- | Convert a 'SeekMode' to a 'Builder'.
showbSeekMode :: SeekMode -> Builder
showbSeekMode = showb
{-# INLINE showbSeekMode #-}

#if MIN_VERSION_base(4,3,0)
-- | Convert a 'TextEncoding' to a 'Builder'.
showbTextEncoding :: TextEncoding -> Builder
showbTextEncoding = fromString . textEncodingName
{-# INLINE showbTextEncoding #-}
#endif

#if MIN_VERSION_base(4,4,0)
-- | Convert a 'CodingProgress' to a 'Builder'.
showbCodingProgress :: CodingProgress -> Builder
showbCodingProgress = showb
{-# INLINE showbCodingProgress #-}

-- | Convert a 'CodingFailureMode' value to a 'Builder'.
showbCodingFailureMode :: CodingFailureMode -> Builder
showbCodingFailureMode = showb
{-# INLINE showbCodingFailureMode #-}
#endif

-- | Convert a 'Newline' to a 'Builder'.
showbNewline :: Newline -> Builder
showbNewline = showb
{-# INLINE showbNewline #-}

-- | Convert a 'NewlineMode' to a 'Builder' with the given precedence.
showbNewlineModePrec :: Int -> NewlineMode -> Builder
showbNewlineModePrec = showbPrec
{-# INLINE showbNewlineModePrec #-}

instance Show Handle where
    showb = showbHandle
    {-# INLINE showb #-}

$(deriveShowPragmas defaultInlineShowb     ''IOMode)
$(deriveShowPragmas defaultInlineShowbPrec ''BufferMode)

instance Show HandlePosn where
    showb = showbHandlePosn
    {-# INLINE showb #-}

$(deriveShowPragmas defaultInlineShowb     ''SeekMode)

#if MIN_VERSION_base(4,3,0)
instance Show TextEncoding where
    showb = showbTextEncoding
    {-# INLINE showb #-}
#endif

#if MIN_VERSION_base(4,4,0)
$(deriveShowPragmas defaultInlineShowb     ''CodingProgress)
$(deriveShowPragmas defaultInlineShowb     ''CodingFailureMode)
#endif

$(deriveShowPragmas defaultInlineShowb     ''Newline)
$(deriveShowPragmas defaultInlineShowbPrec ''NewlineMode)