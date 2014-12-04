{-# LANGUAGE CPP, NoImplicitPrelude, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Show.Text.System.IO
-- Copyright   :  (C) 2014 Ryan Scott
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  Experimental
-- Portability :  GHC
-- 
-- Monomorphic 'Show' functions for 'IO'-related data types.
---------------------------------------------------------------------------- 
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
import GHC.IO.Encoding.Types (TextEncoding(..))
#endif
#if MIN_VERSION_base(4,4,0)
import GHC.IO.Encoding.Failure (CodingFailureMode(..))
import GHC.IO.Encoding.Types (CodingProgress(..))
#endif
import GHC.IO.Handle (HandlePosn(..))
import GHC.IO.Handle.Types (Handle(..))
import GHC.Show (appPrec, appPrec1)

import Prelude hiding (Show)

import System.IO (BufferMode(..), IOMode(..), Newline(..),
                  NewlineMode(..), SeekMode(..))

import Text.Show.Text.Class (Show(showb, showbPrec), showbParen)
import Text.Show.Text.Data.Integral (showbIntegerPrec)
import Text.Show.Text.Data.Maybe (showbMaybePrec)
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
showbIOMode ReadMode      = "ReadMode"
showbIOMode WriteMode     = "WriteMode"
showbIOMode AppendMode    = "AppendMode"
showbIOMode ReadWriteMode = "ReadWriteMode"
{-# INLINE showbIOMode #-}

-- | Convert a 'BufferMode' to a 'Builder' with the given precedence.
showbBufferModePrec :: Int -> BufferMode -> Builder
showbBufferModePrec _ NoBuffering   = "NoBuffering"
showbBufferModePrec _ LineBuffering = "LineBuffering"
showbBufferModePrec p (BlockBuffering size)
    = showbParen (p > appPrec) $ "BlockBuffering " <> showbMaybePrec appPrec1 size
{-# INLINE showbBufferModePrec #-}

-- | Convert a 'HandlePosn' to a 'Builder'.
showbHandlePosn :: HandlePosn -> Builder
showbHandlePosn (HandlePosn h pos)
    = showbHandle h <> " at position " <> showbIntegerPrec 0 pos
{-# INLINE showbHandlePosn #-}

-- | Convert a 'SeekMode' to a 'Builder'.
showbSeekMode :: SeekMode -> Builder
showbSeekMode AbsoluteSeek = "AbsoluteSeek"
showbSeekMode RelativeSeek = "RelativeSeek"
showbSeekMode SeekFromEnd  = "SeekFromEnd"
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
showbCodingProgress InputUnderflow  = "InputUnderflow"
showbCodingProgress OutputUnderflow = "OutputUnderflow"
showbCodingProgress InvalidSequence = "InvalidSequence"
{-# INLINE showbCodingProgress #-}

-- | Convert a 'CodingFailureMode' value to a 'Builder'.
showbCodingFailureMode :: CodingFailureMode -> Builder
showbCodingFailureMode ErrorOnCodingFailure       = "ErrorOnCodingFailure"
showbCodingFailureMode IgnoreCodingFailure        = "IgnoreCodingFailure"
showbCodingFailureMode TransliterateCodingFailure = "TransliterateCodingFailure"
showbCodingFailureMode RoundtripFailure           = "RoundtripFailure"
{-# INLINE showbCodingFailureMode #-}
#endif

-- | Convert a 'Newline' to a 'Builder'.
showbNewline :: Newline -> Builder
showbNewline LF   = "LF"
showbNewline CRLF = "CRLF"
{-# INLINE showbNewline #-}

-- | Convert a 'NewlineMode' to a 'Builder' with the given precedence.
showbNewlineModePrec :: Int -> NewlineMode -> Builder
showbNewlineModePrec p (NewlineMode inl onl) = showbParen (p > appPrec) $
       "NewlineMode {inputNL = "
    <> showbNewline inl
    <> ", outputNL = "
    <> showbNewline onl
    <> s '}'
{-# INLINE showbNewlineModePrec #-}

instance Show Handle where
    showb = showbHandle
    {-# INLINE showb #-}

instance Show IOMode where
    showb = showbIOMode
    {-# INLINE showb #-}

instance Show BufferMode where
    showbPrec = showbBufferModePrec
    {-# INLINE showbPrec #-}

instance Show HandlePosn where
    showb = showbHandlePosn
    {-# INLINE showb #-}

instance Show SeekMode where
    showb = showbSeekMode
    {-# INLINE showb #-}

#if MIN_VERSION_base(4,3,0)
instance Show TextEncoding where
    showb = showbTextEncoding
    {-# INLINE showb #-}
#endif

#if MIN_VERSION_base(4,4,0)
instance Show CodingProgress where
    showb = showbCodingProgress
    {-# INLINE showb #-}

instance Show CodingFailureMode where
    showb = showbCodingFailureMode
    {-# INLINE showb #-}
#endif

instance Show Newline where
    showb = showbNewline
    {-# INLINE showb #-}

instance Show NewlineMode where
    showbPrec = showbNewlineModePrec
    {-# INLINE showbPrec #-}