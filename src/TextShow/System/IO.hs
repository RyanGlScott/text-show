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

'TextShow' instances for 'IO'-related data types.

/Since: 2/
-}
module TextShow.System.IO () where

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
import TextShow.Data.Integral ()
import TextShow.Data.Maybe ()
import TextShow.TH.Internal (deriveTextShow)

#include "inline.h"

-- | /Since: 2/
instance TextShow Handle where
    showb (FileHandle   file _)   = showbHandleFilePath file
    showb (DuplexHandle file _ _) = showbHandleFilePath file
    INLINE_INST_FUN(showb)

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
    INLINE_INST_FUN(showb)

-- | /Since: 2/
$(deriveTextShow ''SeekMode)

-- | /Since: 2/
instance TextShow TextEncoding where
    showb = fromString . textEncodingName
    INLINE_INST_FUN(showb)

#if MIN_VERSION_base(4,4,0)
-- | Only available with @base-4.4.0.0@ or later.
--
-- /Since: 2/
$(deriveTextShow ''CodingProgress)
-- | Only available with @base-4.4.0.0@ or later.
--
-- /Since: 2/
$(deriveTextShow ''CodingFailureMode)
#endif

-- | /Since: 2/
$(deriveTextShow ''Newline)
-- | /Since: 2/
$(deriveTextShow ''NewlineMode)
