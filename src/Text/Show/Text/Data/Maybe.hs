{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Show.Text.Data.Maybe
-- Copyright   :  (C) 2014 Ryan Scott
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  Experimental
-- Portability :  GHC
-- 
-- Monomorphic 'Show' function for 'Maybe' values.
----------------------------------------------------------------------------
module Text.Show.Text.Data.Maybe (showbMaybePrec) where

import Data.Monoid ((<>))
import Data.Text.Lazy.Builder (Builder)

import GHC.Show (appPrec, appPrec1)

import Prelude hiding (Show)

import Text.Show.Text.Class (Show(showbPrec), showbParen)

-- | Convert a 'Maybe' value to a 'Builder' with the given precedence.
showbMaybePrec :: Show a => Int -> Maybe a -> Builder
showbMaybePrec _ Nothing  = "Nothing"
showbMaybePrec p (Just a) = showbParen (p > appPrec) $ "Just " <> showbPrec appPrec1 a
{-# INLINE showbMaybePrec #-}

instance Show a => Show (Maybe a) where
    showbPrec = showbMaybePrec
    {-# INLINE showbPrec #-}