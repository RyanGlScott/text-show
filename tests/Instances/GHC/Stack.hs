{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module:      Instances.GHC.Stack
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'Arbitrary' instances for 'CallStack' and 'SrcLoc'.
-}
module Instances.GHC.Stack () where

-- Ideally, we'd also define these instances for base-4.8.1 and up, but the
-- constructors for CallStack and SrcLoc aren't exposed prior to base-4.9, and
-- the API doesn't provide any way to construct values of those types, so we're
-- pretty much out of luck.
#if MIN_VERSION_base(4,9,0)
import GHC.Stack.Types (CallStack(..), SrcLoc(..))

import Instances.Utils ((<@>))

import Test.QuickCheck (Arbitrary(..), oneof)

instance Arbitrary CallStack where
    arbitrary = oneof [ pure EmptyCallStack
                      , PushCallStack <$> arbitrary <@> EmptyCallStack
                      , pure $ FreezeCallStack EmptyCallStack
                      ]

instance Arbitrary SrcLoc where
    arbitrary = SrcLoc <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                       <*> arbitrary <*> arbitrary <*> arbitrary
#endif
