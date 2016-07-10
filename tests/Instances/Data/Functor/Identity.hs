{-# LANGUAGE CPP                        #-}

#if !(MIN_VERSION_QuickCheck(2,9,0))
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
#endif

{-|
Module:      Instances.Data.Functor.Identity
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'Arbitrary' instance for 'Identity'.
-}
module Instances.Data.Functor.Identity () where

#if !(MIN_VERSION_QuickCheck(2,9,0))
import Data.Functor.Identity (Identity(..))
import Test.QuickCheck (Arbitrary)

deriving instance Arbitrary a => Arbitrary (Identity a)
#endif
