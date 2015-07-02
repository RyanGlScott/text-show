{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module:      Instances.FromStringTextShow
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'Arbitrary' instances 'FromStringShow' and 'FromTextShow'
-}
module Instances.FromStringTextShow () where

import Test.QuickCheck (Arbitrary)
import Text.Show.Text (FromStringShow(..), FromTextShow(..))

deriving instance Arbitrary a => Arbitrary (FromStringShow a)
deriving instance Arbitrary a => Arbitrary (FromTextShow a)
