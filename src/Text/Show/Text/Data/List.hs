{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Show.Text.Data.List
-- Copyright   :  (C) 2014 Ryan Scott
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  Experimental
-- Portability :  GHC
-- 
-- Exports 'showbListDefault'.
----------------------------------------------------------------------------
module Text.Show.Text.Data.List (showbListDefault) where

import Prelude hiding (Show)

import Text.Show.Text.Class (Show(..), showbListDefault)
import Text.Show.Text.Data.Char ()
import Text.Show.Text.Data.Integral ()

instance Show a => Show [a] where
    {-# SPECIALIZE instance Show [String] #-}
    {-# SPECIALIZE instance Show [Char]   #-}
    {-# SPECIALIZE instance Show [Int]    #-}
    showb = showbList
    {-# INLINE showb #-}