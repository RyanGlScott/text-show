{-# LANGUAGE CPP       #-}

#if MIN_VERSION_base(4,6,0) && !(MIN_VERSION_base(4,7,0))
{-# LANGUAGE PolyKinds #-}
#endif

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module:      Instances.GHC.TypeLits
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'Arbitrary' instances for data types in the "GHC.TypeLits" module.
-}
module Instances.GHC.TypeLits () where

#if MIN_VERSION_base(4,6,0)
import GHC.TypeLits

import Prelude ()
import Prelude.Compat

import Test.QuickCheck (Arbitrary(..))

# if MIN_VERSION_base(4,7,0)
import Test.QuickCheck (getNonNegative)
# endif
#endif

#if MIN_VERSION_base(4,7,0)
instance Arbitrary SomeNat where
    arbitrary = do
        nat <- getNonNegative <$> arbitrary
        case someNatVal nat of
             Just sn -> pure sn
             Nothing -> error "Negative natural number" -- Should never happen

instance Arbitrary SomeSymbol where
    arbitrary = someSymbolVal <$> arbitrary
#elif MIN_VERSION_base(4,6,0)
instance SingI a => Arbitrary (Sing a) where
    arbitrary = pure sing

instance SingI n => Arbitrary (IsZero n) where
    arbitrary = pure $ isZero sing

instance SingI n => Arbitrary (IsEven n) where
    arbitrary = pure $ isEven sing
#endif
