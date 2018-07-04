{-# LANGUAGE CPP #-}
#if MIN_VERSION_base(4,12,0)
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
#endif
{-|
Module:      Instances.Data.Monoid
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'Arbitrary' instance for 'Ap'.
-}
module Instances.Data.Monoid () where

#if MIN_VERSION_base(4,12,0)
import Data.Monoid (Ap(..))
import Test.QuickCheck (Arbitrary)

deriving instance Arbitrary (f a) => Arbitrary (Ap f a)
#endif
