{-# LANGUAGE NoImplicitPrelude #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Show.Text
-- Copyright   :  (C) 2014 Ryan Scott
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  Experimental
-- Portability :  GHC
-- 
-- Efficiently convert from values to 'Text' via 'Builder's.
----------------------------------------------------------------------------
module Text.Show.Text (
      -- * The 'Show' class
      Show (..)
    , show
    , showLazy
    , showbParen
      -- * 'Builder's
    , module Data.Text.Lazy.Builder
    , lengthB
    , replicateB
    , unlinesB
    , unwordsB
      -- * Printing values
    , print
    , printLazy
    ) where

import Data.Text.Lazy.Builder

import Prelude hiding (Show(show), print)

import Text.Show.Text.Char ()
import Text.Show.Text.Class
import Text.Show.Text.Containers ()
import Text.Show.Text.Float ()
import Text.Show.Text.Functions (lengthB, replicateB, unlinesB, unwordsB)
import Text.Show.Text.Instances ()
import Text.Show.Text.Int ()
import Text.Show.Text.Ptr ()
import Text.Show.Text.Tuple ()