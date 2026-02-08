{-# OPTIONS_GHC -Wno-orphans #-}
{-|
Module:      Instances.Data.Text.Builder.Linear
Copyright:   (C) 2026 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

An 'Arbitrary' instance for @text-builder-linear@'s 'Builder' type.
-}
module Instances.Data.Text.Builder.Linear () where

import Data.Text.Builder.Linear (Builder, fromText)

import Instances.Data.Text ()

import Test.QuickCheck (Arbitrary(..))

instance Arbitrary Builder where
  arbitrary = fromText <$> arbitrary
