{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module:      Instances.Options
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'Arbitrary' instance for 'Options' and related datatypes.
-}
module Instances.Options () where

import Test.QuickCheck (Arbitrary(..), arbitraryBoundedEnum)
import TextShow.TH (Options(..), GenTextMethods)

deriving instance Arbitrary Options

instance Arbitrary GenTextMethods where
    arbitrary = arbitraryBoundedEnum
