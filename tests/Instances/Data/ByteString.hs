{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module:      Instances.Data.ByteString
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

'Arbitrary' instance for 'ShortByteString'.
-}
module Instances.Data.ByteString () where

import Data.ByteString.Short (ShortByteString, pack)

import Prelude ()
import Prelude.Compat

import Test.QuickCheck (Arbitrary(..))

instance Arbitrary ShortByteString where
    arbitrary = pack <$> arbitrary
