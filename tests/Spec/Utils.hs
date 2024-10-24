{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}

#if __GLASGOW_HASKELL__ >= 806
{-# LANGUAGE QuantifiedConstraints #-}
#endif

{-|
Module:      Spec.Utils
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Testing-related utility functions.
-}
module Spec.Utils (
      matchesTextShowSpec
    , prop_matchesTextShow
    , matchesTextShow1Spec
    , matchesTextShow2Spec
    , genericTextShowSpec
    , genericTextShow1Spec

    , Some(..)
    , GArbitrary(..)
    ) where

import Data.Functor.Classes (Show1, Show2, showsPrec1, showsPrec2)
import Data.Proxy (Proxy(..))

import GHC.Generics

import Test.Hspec (Expectation, Spec, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary(..), Gen)

import TextShow (TextShow(..), TextShow1(..), TextShow2(..),
                 showbPrec1, showbPrec2, fromString)
import TextShow.Generic

#if __GLASGOW_HASKELL__ >= 806
import GHC.Show (appPrec, appPrec1)
import TextShow (showbParen, showbSpace)
#endif

-- | Expect a type's 'Show' instances to coincide for both 'String's and 'Text',
-- irrespective of precedence.
matchesTextShowSpec :: forall a. (Arbitrary a, Show a, TextShow a)
                    => Proxy a -> Spec
matchesTextShowSpec _ = prop "TextShow instance" (prop_matchesTextShow :: Int -> a -> Expectation)

-- | Verifies that a type's 'Show' instances coincide for both 'String's and 'Text',
-- irrespective of precedence.
prop_matchesTextShow :: (Show a, TextShow a) => Int -> a -> Expectation
prop_matchesTextShow p x = showbPrec p x `shouldBe` fromString (showsPrec p x "")

-- | Expect a type's 'Show1' instances to coincide for both 'String's and 'Text',
-- irrespective of precedence.
matchesTextShow1Spec :: forall f a.
                        (Arbitrary (f a), Show1 f, Show a, Show (f a), TextShow1 f, TextShow a)
                     => Proxy (f a) -> Spec
matchesTextShow1Spec _ = prop "TextShow1 instance" (prop_matchesTextShow1 :: Int -> f a -> Expectation)

-- | Verifies that a type's 'Show1' instances coincide for both 'String's and 'Text',
-- irrespective of precedence.
prop_matchesTextShow1 :: (Show1 f, Show a, TextShow1 f, TextShow a) => Int -> f a -> Expectation
prop_matchesTextShow1 p x = showbPrec1 p x `shouldBe` fromString (showsPrec1 p x "")

-- | Expect a type's 'Show2' instances to coincide for both 'String's and 'Text',
-- irrespective of precedence.
matchesTextShow2Spec :: forall f a b.
                        (Arbitrary (f a b), Show2 f, Show a, Show b, Show (f a b),
                         TextShow2 f, TextShow a, TextShow b)
                     => Proxy (f a b) -> Spec
matchesTextShow2Spec _ = prop "TextShow2 instance" (prop_matchesTextShow2 :: Int -> f a b -> Expectation)

-- | Verifies that a type's 'Show2' instances coincide for both 'String's and 'Text',
-- irrespective of precedence.
prop_matchesTextShow2 :: (Show2 f, Show a, Show b, TextShow2 f, TextShow a, TextShow b)
                      => Int -> f a b -> Expectation
prop_matchesTextShow2 p x = showbPrec2 p x `shouldBe` fromString (showsPrec2 p x "")

-- | Expect a type's 'TextShow' instance to coincide with the output produced
-- by the equivalent 'Generic' functions.
genericTextShowSpec :: forall a. (Arbitrary a, Show a, TextShow a,
                                  Generic a, GTextShowB (Rep a ()))
                    => Proxy a -> Spec
genericTextShowSpec _ = prop "generic TextShow" (prop_genericTextShow  :: Int -> a -> Expectation)

-- | Verifies that a type's 'TextShow' instance coincides with the output produced
-- by the equivalent 'Generic' functions.
prop_genericTextShow :: (TextShow a, Generic a, GTextShowB (Rep a ()))
                     => Int -> a -> Expectation
prop_genericTextShow p x = showbPrec p x `shouldBe` genericShowbPrec p x

-- | Expect a type's 'TextShow1' instance to coincide with the output produced
-- by the equivalent 'Generic1' functions.
genericTextShow1Spec :: forall f a. (Arbitrary (f a), Show (f a), TextShow1 f,
                                     Generic1 f, GTextShowB1 (Rep1 f), TextShow a)
                     => Proxy (f a) -> Spec
genericTextShow1Spec _ = prop "generic TextShow1" (prop_genericTextShow1 :: Int -> f a -> Expectation)

-- | Verifies that a type's 'TextShow1' instance coincides with the output produced
-- by the equivalent 'Generic1' functions.
prop_genericTextShow1 :: ( TextShow1 f, Generic1 f
                         , GTextShowB1 (Rep1 f), TextShow a
                         )
                      => Int -> f a -> Expectation
prop_genericTextShow1 p x =
    showbPrec1 p x `shouldBe` genericLiftShowbPrec showbPrec showbList p x

-- | A data type that existentially closes over something.
data Some t where
  Some :: t a -> Some t

#if __GLASGOW_HASKELL__ >= 806
deriving instance (forall a. Show (t a)) => Show (Some t)
instance (forall a. TextShow (t a)) => TextShow (Some t) where
  showbPrec p (Some x) =
    showbParen (p > appPrec) $
    fromString "Some" <> showbSpace <> showbPrec appPrec1 x
#endif

instance GArbitrary t => Arbitrary (Some t) where
  arbitrary = garbitrary

-- | An 'Arbitrary'-like class for 1-type-parameter GADTs.
class GArbitrary t where
  garbitrary :: Gen (Some t)
