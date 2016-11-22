{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module:      Spec.Utils
Copyright:   (C) 2014-2016 Ryan Scott
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
    , prop_matchesTextShow1
#if defined(NEW_FUNCTOR_CLASSES)
    , matchesTextShow2Spec
    , prop_matchesTextShow2
#endif
    , genericTextShowSpec
    , prop_genericTextShow
    , genericTextShow1Spec
    , prop_genericTextShow1
    ) where

import Data.Functor.Classes (Show1, showsPrec1)
import Data.Proxy (Proxy(..))

import Generics.Deriving.Base

import Test.Hspec (Spec)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary)

import TextShow (TextShow(..), TextShow1(..), showbPrec1, fromString)
import TextShow.Generic

#if defined(NEW_FUNCTOR_CLASSES)
import Data.Functor.Classes (Show2, showsPrec2)
import TextShow (TextShow2(..), showbPrec2)
#endif

-- | Expect a type's 'Show' instances to coincide for both 'String's and 'Text',
-- irrespective of precedence.
matchesTextShowSpec :: forall a. (Arbitrary a, Show a, TextShow a)
                    => Proxy a -> Spec
matchesTextShowSpec _ = prop "TextShow instance" (prop_matchesTextShow :: Int -> a -> Bool)

-- | Verifies that a type's 'Show' instances coincide for both 'String's and 'Text',
-- irrespective of precedence.
prop_matchesTextShow :: (Show a, TextShow a) => Int -> a -> Bool
prop_matchesTextShow p x = fromString (showsPrec p x "") == showbPrec p x

-- | Expect a type's 'Show1' instances to coincide for both 'String's and 'Text',
-- irrespective of precedence.
matchesTextShow1Spec :: forall f a.
                        (Arbitrary (f a), Show1 f, Show a, Show (f a), TextShow1 f, TextShow a)
                     => Proxy (f a) -> Spec
matchesTextShow1Spec _ = prop "TextShow1 instance" (prop_matchesTextShow1 :: Int -> f a -> Bool)

-- | Verifies that a type's 'Show1' instances coincide for both 'String's and 'Text',
-- irrespective of precedence.
prop_matchesTextShow1 :: (Show1 f, Show a, TextShow1 f, TextShow a) => Int -> f a -> Bool
prop_matchesTextShow1 p x = fromString (showsPrec1 p x "") == showbPrec1 p x

#if defined(NEW_FUNCTOR_CLASSES)
-- | Expect a type's 'Show2' instances to coincide for both 'String's and 'Text',
-- irrespective of precedence.
matchesTextShow2Spec :: forall f a b.
                        (Arbitrary (f a b), Show2 f, Show a, Show b, Show (f a b),
                         TextShow2 f, TextShow a, TextShow b)
                     => Proxy (f a b) -> Spec
matchesTextShow2Spec _ = prop "TextShow2 instance" (prop_matchesTextShow2 :: Int -> f a b -> Bool)

-- | Verifies that a type's 'Show2' instances coincide for both 'String's and 'Text',
-- irrespective of precedence.
prop_matchesTextShow2 :: (Show2 f, Show a, Show b, TextShow2 f, TextShow a, TextShow b)
                      => Int -> f a b -> Bool
prop_matchesTextShow2 p x = fromString (showsPrec2 p x "") == showbPrec2 p x
#endif

-- | Expect a type's 'TextShow' instance to coincide with the output produced
-- by the equivalent 'Generic' functions.
genericTextShowSpec :: forall a. (Arbitrary a, Show a, TextShow a,
                                  Generic a, GTextShowB Zero (Rep a))
                    => Proxy a -> Spec
genericTextShowSpec _ = prop "generic TextShow" (prop_genericTextShow  :: Int -> a -> Bool)

-- | Verifies that a type's 'TextShow' instance coincides with the output produced
-- by the equivalent 'Generic' functions.
prop_genericTextShow :: (TextShow a, Generic a, GTextShowB Zero (Rep a))
                     => Int -> a -> Bool
prop_genericTextShow p x = showbPrec p x == genericShowbPrec p x

-- | Expect a type's 'TextShow1' instance to coincide with the output produced
-- by the equivalent 'Generic1' functions.
genericTextShow1Spec :: forall f a. (Arbitrary (f a), Show (f a), TextShow1 f,
                                     Generic1 f, GTextShowB One (Rep1 f), TextShow a)
                     => Proxy (f a) -> Spec
genericTextShow1Spec _ = prop "generic TextShow1" (prop_genericTextShow1 :: Int -> f a -> Bool)

-- | Verifies that a type's 'TextShow1' instance coincides with the output produced
-- by the equivalent 'Generic1' functions.
prop_genericTextShow1 :: ( TextShow1 f, Generic1 f
                         , GTextShowB One (Rep1 f), TextShow a
                         )
                      => Int -> f a -> Bool
prop_genericTextShow1 p x =
    showbPrec1 p x == genericLiftShowbPrec showbPrec showbList p x
