{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-orphans      #-}

{-|
Module:      Instances.Data.Text
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'Arbitrary' instances for data types in the @text@ library.
-}
module Instances.Data.Text () where

import Data.Text.Encoding (Decoding(..))
import Data.Text.Encoding.Error (UnicodeException(..))
import Data.Text.Internal.Fusion.Size (Size, exactSize)
import Data.Text.Lazy.Builder (Builder, fromString)

import GHC.Generics (Generic)

import Instances.Utils ((<@>))
import Instances.Utils.GenericArbitrary (genericArbitrary)

import Prelude ()
import Prelude.Compat

import Test.QuickCheck (Arbitrary(..), getNonNegative)
import Test.QuickCheck.Instances ()

instance Arbitrary Builder where
    arbitrary = fromString <$> arbitrary

instance Arbitrary UnicodeException where
    arbitrary = genericArbitrary

instance Arbitrary Decoding where
    arbitrary = Some <$> arbitrary <*> arbitrary <@> undefined

instance Arbitrary Size where
    arbitrary = exactSize . getNonNegative <$> arbitrary

deriving instance Generic UnicodeException
