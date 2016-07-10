{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module:      Instances.Data.List.NonEmpty
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'Arbitrary' instance for 'NonEmpty'.
-}
module Instances.Data.List.NonEmpty () where

import Data.List.NonEmpty (NonEmpty(..))

import Text.Show.Deriving (deriveShow1)

#if !(MIN_VERSION_QuickCheck(2,9,0))
import Prelude ()
import Prelude.Compat

import Test.QuickCheck (Arbitrary(..))

instance Arbitrary a => Arbitrary (NonEmpty a) where
    arbitrary = (:|) <$> arbitrary <*> arbitrary
#endif

-- TODO: Replace this with a non-orphan instance
$(deriveShow1 ''NonEmpty)
