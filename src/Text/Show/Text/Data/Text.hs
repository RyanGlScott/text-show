{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Show.Text.Data.Text
-- Copyright   :  (C) 2014 Ryan Scott
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  Experimental
-- Portability :  GHC
-- 
-- Monomorphic 'Show' functions for 'Text' types.
----------------------------------------------------------------------------
module Text.Show.Text.Data.Text (
      showbTextStrict
    , showbTextLazy
    ) where

import Data.Text      as TS
import Data.Text.Lazy as TL
import Data.Text.Lazy.Builder (Builder, toLazyText)

import Prelude hiding (Show)

import Text.Show.Text.Class (Show(showb))
import Text.Show.Text.Data.Char ()
import Text.Show.Text.Data.List ()

-- | Convert a strict 'Text' into a 'Builder'.
showbTextStrict :: TS.Text -> Builder
showbTextStrict = showb . TS.unpack

-- | Convert a lazy 'Text' into a 'Builder'.
showbTextLazy :: TL.Text -> Builder
showbTextLazy = showb . TL.unpack

instance Show Builder where
    showb = showb . toLazyText
    {-# INLINE showb #-}

-- Strict variant
instance Show TS.Text where
    showb = showbTextStrict
    {-# INLINE showb #-}

-- Lazy variant
instance Show TL.Text where
    showb = showbTextLazy
    {-# INLINE showb #-}