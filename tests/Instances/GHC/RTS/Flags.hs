{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module:      Instances.GHC.RTS.Flags
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'Arbitrary' instances for data types in the "GHC.RTS.Flags" module.
-}
module Instances.GHC.RTS.Flags () where

#if MIN_VERSION_base(4,8,0)
import GHC.RTS.Flags
import Test.QuickCheck (Arbitrary(..))

instance Arbitrary ConcFlags where
    arbitrary = ConcFlags <$> arbitrary <*> arbitrary

instance Arbitrary MiscFlags where
    arbitrary = MiscFlags <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary DebugFlags where
    arbitrary = DebugFlags <$> arbitrary <*> arbitrary <*> arbitrary
                           <*> arbitrary <*> arbitrary <*> arbitrary
                           <*> arbitrary <*> arbitrary <*> arbitrary
                           <*> arbitrary <*> arbitrary <*> arbitrary
                           <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary TickyFlags where
    arbitrary = TickyFlags <$> arbitrary <*> arbitrary
#endif
