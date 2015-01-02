{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.Data.Text
Copyright:   (C) 2014 Ryan Scott
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
    ) where

import Data.Text      as TS
import Data.Text.Lazy as TL
import Data.Text.Lazy.Builder (Builder, toLazyText)

import Prelude hiding (Show)

import Text.Show.Text.Classes (Show(showb))
import Text.Show.Text.Data.Char (showbString)

#include "inline.h"

-- | Convert a strict 'TS.Text' into a 'Builder'.
-- 'showbText' should not be confused with @fromText@, as 'showbText' escapes
-- certain characters (such as double quotes).
-- 
-- /Since: 0.5/
showbText :: TS.Text -> Builder
showbText = showbString . TS.unpack
{-# INLINE showbText #-}

-- | Convert a lazy 'TL.Text' into a 'Builder'.
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

instance Show TS.Text where
    showb = showbText
    INLINE_INST_FUN(showb)

instance Show TL.Text where
    showb = showbTextLazy
    INLINE_INST_FUN(showb)

instance Show Builder where
    showb = showbBuilder
    INLINE_INST_FUN(showb)