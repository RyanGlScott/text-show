{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}

#if MIN_VERSION_base(4,8,0)
{-# LANGUAGE FlexibleContexts           #-}
#endif

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module:      Instances.Data.Monoid
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'Arbitrary' instances for data types in the "Data.Monoid" module.
-}
module Instances.Data.Monoid () where

import Data.Monoid
import Test.QuickCheck (Arbitrary)

deriving instance Arbitrary All
deriving instance Arbitrary Any
deriving instance Arbitrary a => Arbitrary (Dual a)
deriving instance Arbitrary a => Arbitrary (First a)
deriving instance Arbitrary a => Arbitrary (Last a)
deriving instance Arbitrary a => Arbitrary (Product a)
deriving instance Arbitrary a => Arbitrary (Sum a)
#if MIN_VERSION_base(4,8,0)
deriving instance Arbitrary (f a) => Arbitrary (Alt f a)
#endif
