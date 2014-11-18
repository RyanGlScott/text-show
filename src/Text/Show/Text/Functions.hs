{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Show.Text.Functions
-- Copyright   :  (C) 2014 Ryan Scott
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  Experimental
-- Portability :  GHC
-- 
-- Optional 'Show' instance for functions.
----------------------------------------------------------------------------
module Text.Show.Text.Functions (showbFunction) where

import Data.Text.Lazy.Builder (Builder)
import Prelude hiding (Show)
import Text.Show.Text.Class (Show(showb))

-- | Convert a function to a 'Builder'.
showbFunction :: (a -> b) -> Builder
showbFunction = const "<function>"
{-# INLINE showbFunction #-}

instance Show (a -> b) where
    showb = showbFunction
    {-# INLINE showb #-}