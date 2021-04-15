{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module:      Instances.Data.Tuple
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'Arbitrary' instances for tuple types.
-}
module Instances.Data.Tuple () where

import Data.Orphans ()
import Instances.Utils.GenericArbitrary (genericArbitrary)
import Test.QuickCheck (Arbitrary(..))

#if !(MIN_VERSION_base(4,16,0))
import GHC.Generics (Generic)
#endif

instance ( Arbitrary a
         , Arbitrary b
         , Arbitrary c
         , Arbitrary d
         , Arbitrary e
         , Arbitrary f
         , Arbitrary g
         , Arbitrary h
         , Arbitrary i
         , Arbitrary j
         , Arbitrary k
         ) => Arbitrary (a, b, c, d, e, f, g, h, i, j, k) where
    arbitrary = genericArbitrary

instance ( Arbitrary a
         , Arbitrary b
         , Arbitrary c
         , Arbitrary d
         , Arbitrary e
         , Arbitrary f
         , Arbitrary g
         , Arbitrary h
         , Arbitrary i
         , Arbitrary j
         , Arbitrary k
         , Arbitrary l
         ) => Arbitrary (a, b, c, d, e, f, g, h, i, j, k, l) where
    arbitrary = genericArbitrary

instance ( Arbitrary a
         , Arbitrary b
         , Arbitrary c
         , Arbitrary d
         , Arbitrary e
         , Arbitrary f
         , Arbitrary g
         , Arbitrary h
         , Arbitrary i
         , Arbitrary j
         , Arbitrary k
         , Arbitrary l
         , Arbitrary m
         ) => Arbitrary (a, b, c, d, e, f, g, h, i, j, k, l, m) where
    arbitrary = genericArbitrary

instance ( Arbitrary a
         , Arbitrary b
         , Arbitrary c
         , Arbitrary d
         , Arbitrary e
         , Arbitrary f
         , Arbitrary g
         , Arbitrary h
         , Arbitrary i
         , Arbitrary j
         , Arbitrary k
         , Arbitrary l
         , Arbitrary m
         , Arbitrary n
         ) => Arbitrary (a, b, c, d, e, f, g, h, i, j, k, l, m, n) where
    arbitrary = genericArbitrary

instance ( Arbitrary a
         , Arbitrary b
         , Arbitrary c
         , Arbitrary d
         , Arbitrary e
         , Arbitrary f
         , Arbitrary g
         , Arbitrary h
         , Arbitrary i
         , Arbitrary j
         , Arbitrary k
         , Arbitrary l
         , Arbitrary m
         , Arbitrary n
         , Arbitrary o
         ) => Arbitrary (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) where
    arbitrary = genericArbitrary

-- TODO: Replace these instances with generic-deriving
#if !(MIN_VERSION_base(4,16,0))
deriving instance Generic (a, b, c, d, e, f, g, h, i, j, k)
deriving instance Generic (a, b, c, d, e, f, g, h, i, j, k, l)
deriving instance Generic (a, b, c, d, e, f, g, h, i, j, k, l, m)
deriving instance Generic (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
deriving instance Generic (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
#endif
