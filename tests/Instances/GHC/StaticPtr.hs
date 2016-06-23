{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module:      Instances.GHC.StaticPtr
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'Arbitrary' instance for 'StaticPtrInfo'.
-}
module Instances.GHC.StaticPtr () where

#if MIN_VERSION_base(4,8,0)
import GHC.StaticPtr (StaticPtrInfo(..))
import Test.QuickCheck (Arbitrary(..))

instance Arbitrary StaticPtrInfo where
    arbitrary = StaticPtrInfo <$> arbitrary
                              <*> arbitrary
                              <*> arbitrary
# if __GLASGOW_HASKELL__ < 801
                              <*> arbitrary
# endif
#endif
