{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{-|
Module:      Instances.GHC.Stack
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'Arbitrary' instances for 'CallStack' and 'SrcLoc'.
-}
module Instances.GHC.Stack () where

import           GHC.Stack.Types (CallStack(..), SrcLoc(..))
import           Instances.Utils ((<@>))
import           Test.QuickCheck (oneof)

#if !(MIN_VERSION_base(4,15,0))
import           GHC.Generics (Generic)
#endif

import           Instances.Utils.GenericArbitrary (genericArbitrary)

import           Test.QuickCheck (Arbitrary(..))

#if !(MIN_VERSION_base(4,15,0))
deriving instance Generic SrcLoc
#endif

instance Arbitrary CallStack where
    arbitrary = oneof [ pure EmptyCallStack
                      , PushCallStack <$> arbitrary <*> arbitrary <@> EmptyCallStack
                      , pure $ FreezeCallStack EmptyCallStack
                      ]

instance Arbitrary SrcLoc where
    arbitrary = genericArbitrary
