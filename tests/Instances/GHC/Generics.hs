{-# LANGUAGE CPP                        #-}

#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeOperators              #-}
#endif

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module:      Instances.GHC.Generics
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'Arbitrary' instances for data types in the "GHC.Generics" module.
-}
module Instances.GHC.Generics () where

#if __GLASGOW_HASKELL__ >= 702
import GHC.Generics (U1(..), Par1(..), Rec1(..), K1(..),
                     M1(..), (:+:)(..), (:*:)(..), (:.:)(..),
                     Fixity(..), Associativity(..), Arity(..))

import Prelude ()
import Prelude.Compat

import Test.QuickCheck (Arbitrary(..), arbitraryBoundedEnum, oneof)

instance Arbitrary (U1 p) where
    arbitrary = pure U1

deriving instance Arbitrary p         => Arbitrary (Par1 p)
deriving instance Arbitrary (f p)     => Arbitrary (Rec1 f p)
deriving instance Arbitrary c         => Arbitrary (K1 i c p)
deriving instance Arbitrary (f p)     => Arbitrary (M1 i c f p)
deriving instance Arbitrary (f (g p)) => Arbitrary ((f :.: g) p)

instance (Arbitrary (f p), Arbitrary (g p)) => Arbitrary ((f :+: g) p) where
    arbitrary = oneof [L1 <$> arbitrary, R1 <$> arbitrary]

instance (Arbitrary (f p), Arbitrary (g p)) => Arbitrary ((f :*: g) p) where
    arbitrary = (:*:) <$> arbitrary <*> arbitrary

instance Arbitrary Fixity where
    arbitrary = oneof [pure Prefix, Infix <$> arbitrary <*> arbitrary]

deriving instance Bounded Associativity
deriving instance Enum Associativity
instance Arbitrary Associativity where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary Arity where
    arbitrary = oneof [pure NoArity, Arity <$> arbitrary]
#endif
