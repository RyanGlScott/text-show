{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeSynonymInstances       #-}
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

import Generics.Deriving.Base

import GHC.Exts (Char(C#), Double(D#), Float(F#), Int(I#), Word(W#))

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

#if MIN_VERSION_base(4,9,0)
deriving instance Bounded SourceUnpackedness
deriving instance Enum SourceUnpackedness
instance Arbitrary SourceUnpackedness where
    arbitrary = arbitraryBoundedEnum

deriving instance Bounded SourceStrictness
deriving instance Enum SourceStrictness
instance Arbitrary SourceStrictness where
    arbitrary = arbitraryBoundedEnum

deriving instance Bounded DecidedStrictness
deriving instance Enum DecidedStrictness
instance Arbitrary DecidedStrictness where
    arbitrary = arbitraryBoundedEnum
#else
instance Arbitrary Arity where
    arbitrary = oneof [pure NoArity, Arity <$> arbitrary]
#endif

instance Arbitrary (UChar p) where
    arbitrary = do
        C# c <- arbitrary
        pure $ UChar c

instance Arbitrary (UDouble p) where
    arbitrary = do
        D# d <- arbitrary
        pure $ UDouble d

instance Arbitrary (UFloat p) where
    arbitrary = do
        F# f <- arbitrary
        pure $ UFloat f

instance Arbitrary (UInt p) where
    arbitrary = do
        I# i <- arbitrary
        pure $ UInt i

instance Arbitrary (UWord p) where
    arbitrary = do
        W# w <- arbitrary
        pure $ UWord w
