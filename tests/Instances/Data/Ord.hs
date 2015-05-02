{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module:      Instances.Data.Ord
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

'Arbitrary' instance for 'Down'.
-}
module Instances.Data.Ord () where

#if MIN_VERSION_base(4,6,0)
import Data.Ord (Down(..))
import Test.QuickCheck (Arbitrary)

deriving instance Arbitrary a => Arbitrary (Down a)
#endif
