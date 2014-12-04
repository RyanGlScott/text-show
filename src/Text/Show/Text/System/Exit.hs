{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Show.Text.Data.Monoid
-- Copyright   :  (C) 2014 Ryan Scott
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  Experimental
-- Portability :  GHC
-- 
-- Monomorphic 'Show' function for 'ExitCode'.
----------------------------------------------------------------------------
module Text.Show.Text.System.Exit (showbExitCodePrec) where

import Data.Text.Lazy.Builder (Builder)

import GHC.Show (appPrec, appPrec1)

import Prelude hiding (Show)

import System.Exit (ExitCode(..))

import Text.Show.Text.Class (Show(showbPrec), showbParen)
import Text.Show.Text.Data.Integral (showbIntPrec)
import Text.Show.Text.Utils ((<>))

-- | Convert an 'ExitCode' to a 'Builder' with the given precedence.
showbExitCodePrec :: Int -> ExitCode -> Builder
showbExitCodePrec _ ExitSuccess     = "ExitSuccess"
showbExitCodePrec p (ExitFailure c) = showbParen (p > appPrec) $
    "ExitFailure " <> showbIntPrec appPrec1 c
{-# INLINE showbExitCodePrec #-}

instance Show ExitCode where
    showbPrec = showbExitCodePrec
    {-# INLINE showbPrec #-}