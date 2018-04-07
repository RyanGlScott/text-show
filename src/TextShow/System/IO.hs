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

'TextShow' instances for 'IO'-related data types.

/Since: 2/
-}
module TextShow.System.IO () where

import Data.Text.Lazy.Builder (Builder, fromString, singleton)

import GHC.IO.Encoding.Failure (CodingFailureMode)
import GHC.IO.Encoding.Types (CodingProgress, TextEncoding(textEncodingName))
import GHC.IO.Handle (HandlePosn(..))
import GHC.IO.Handle.Types (Handle(..))

import Prelude ()
import Prelude.Compat

import System.IO (BufferMode, IOMode, Newline, NewlineMode, SeekMode)

import TextShow.Classes (TextShow(..))
import TextShow.Data.Integral ()
import TextShow.Data.Maybe ()
import TextShow.TH.Internal (deriveTextShow)

-- | /Since: 2/
instance TextShow Handle where
    showb (FileHandle   file _)   = showbHandleFilePath file
    showb (DuplexHandle file _ _) = showbHandleFilePath file
    {-# INLINE showb #-}

-- | Convert a 'Handle`'s 'FilePath' to a 'Builder'.
showbHandleFilePath :: FilePath -> Builder
showbHandleFilePath file = "{handle: " <> fromString file <> singleton '}'
{-# INLINE showbHandleFilePath #-}

-- | /Since: 2/
$(deriveTextShow ''IOMode)
-- | /Since: 2/
$(deriveTextShow ''BufferMode)

-- | /Since: 2/
instance TextShow HandlePosn where
    showb (HandlePosn h pos) = showb h <> " at position " <> showbPrec 0 pos
    {-# INLINE showb #-}

-- | /Since: 2/
$(deriveTextShow ''SeekMode)

-- | /Since: 2/
instance TextShow TextEncoding where
    showb = fromString . textEncodingName
    {-# INLINE showb #-}

-- | /Since: 2/
$(deriveTextShow ''CodingProgress)
-- | /Since: 2/
$(deriveTextShow ''CodingFailureMode)
-- | /Since: 2/
$(deriveTextShow ''Newline)
-- | /Since: 2/
$(deriveTextShow ''NewlineMode)
