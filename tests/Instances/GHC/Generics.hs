{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module:      Instances.GHC.Generics
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'Arbitrary' instances for data types in the "GHC.Generics" module.
-}
module Instances.GHC.Generics () where

import Data.Orphans ()
import Generics.Deriving.Base
import Instances.Utils.GenericArbitrary (genericArbitrary)
import Test.QuickCheck (Arbitrary(..), arbitraryBoundedEnum)

instance Arbitrary (U1 p) where
    arbitrary = genericArbitrary

deriving instance Arbitrary p         => Arbitrary (Par1 p)
deriving instance Arbitrary (f p)     => Arbitrary (Rec1 f p)
deriving instance Arbitrary c         => Arbitrary (K1 i c p)
deriving instance Arbitrary (f p)     => Arbitrary (M1 i c f p)
deriving instance Arbitrary (f (g p)) => Arbitrary ((f :.: g) p)

instance (Arbitrary (f p), Arbitrary (g p)) => Arbitrary ((f :+: g) p) where
    arbitrary = genericArbitrary

instance (Arbitrary (f p), Arbitrary (g p)) => Arbitrary ((f :*: g) p) where
    arbitrary = genericArbitrary

instance Arbitrary Fixity where
    arbitrary = genericArbitrary

instance Arbitrary Associativity where
    arbitrary = arbitraryBoundedEnum

#if MIN_VERSION_base(4,9,0)
instance Arbitrary SourceUnpackedness where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary SourceStrictness where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary DecidedStrictness where
    arbitrary = arbitraryBoundedEnum
#else
instance Arbitrary Arity where
    arbitrary = genericArbitrary
#endif

instance Arbitrary (UChar p) where
    arbitrary = genericArbitrary

instance Arbitrary (UDouble p) where
    arbitrary = genericArbitrary

instance Arbitrary (UFloat p) where
    arbitrary = genericArbitrary

instance Arbitrary (UInt p) where
    arbitrary = genericArbitrary

instance Arbitrary (UWord p) where
    arbitrary = genericArbitrary
