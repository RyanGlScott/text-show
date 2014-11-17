{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Show.Text.Data.Either
-- Copyright   :  (C) 2014 Ryan Scott
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  Experimental
-- Portability :  GHC
-- 
-- Monomorphic 'Show' function for 'Either' values.
----------------------------------------------------------------------------
module Text.Show.Text.Data.Either (showbEitherPrec) where

import Data.Monoid ((<>))
import Data.Text.Lazy.Builder (Builder)

import GHC.Show (appPrec, appPrec1)

import Prelude hiding (Show)

import Text.Show.Text.Class (Show(showbPrec), showbParen)

-- | Convert a 'Either' value to a 'Builder' with the given precedence.
showbEitherPrec :: (Show a, Show b) => Int -> Either a b -> Builder
showbEitherPrec p (Left a)  = showbParen (p > appPrec) $ "Left "  <> showbPrec appPrec1 a
showbEitherPrec p (Right b) = showbParen (p > appPrec) $ "Right " <> showbPrec appPrec1 b
{-# INLINE showbEitherPrec #-}

instance (Show a, Show b) => Show (Either a b) where
    showbPrec = showbEitherPrec
    {-# INLINE showbPrec #-}