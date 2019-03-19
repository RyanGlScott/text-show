{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-orphans      #-}

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

import Data.Text.Encoding.Error (UnicodeException(..))
import Data.Text.Lazy.Builder (Builder, fromString)

#if MIN_VERSION_text(1,0,0)
import Data.Text.Encoding (Decoding(..))
import Instances.Utils ((<@>))
#endif

#if MIN_VERSION_text(1,1,0)
import Data.Text.Internal.Fusion.Size (Size, exactSize)
import Test.QuickCheck (getNonNegative)
#endif

import GHC.Generics (Generic)

import Instances.Utils.GenericArbitrary (genericArbitrary)

import Prelude ()
import Prelude.Compat

import Test.QuickCheck (Arbitrary(..))
import Test.QuickCheck.Instances ()

instance Arbitrary Builder where
    arbitrary = fromString <$> arbitrary

instance Arbitrary UnicodeException where
    arbitrary = genericArbitrary

#if MIN_VERSION_text(1,0,0)
instance Arbitrary Decoding where
    arbitrary = Some <$> arbitrary <*> arbitrary <@> undefined
#endif

#if MIN_VERSION_text(1,1,0)
instance Arbitrary Size where
    arbitrary = exactSize . getNonNegative <$> arbitrary
#endif

deriving instance Generic UnicodeException
