{-# LANGUAGE CPP             #-}

#if MIN_VERSION_base(4,8,1)
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
#endif

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

#if MIN_VERSION_base(4,8,1)
import qualified Generics.Deriving.TH as Generics (deriveAll0)
# if MIN_VERSION_base(4,9,0)
import           GHC.Stack.Types (CallStack(..), SrcLoc(..))
import           Instances.Utils ((<@>))
import           Test.QuickCheck (oneof)
# else
import           GHC.SrcLoc (SrcLoc)
import           GHC.Stack (CallStack)
# endif

import           Instances.Utils.GenericArbitrary (genericArbitrary)

import           Test.QuickCheck (Arbitrary(..))

# if !(MIN_VERSION_base(4,9,0))
$(Generics.deriveAll0 ''CallStack)
# endif
$(Generics.deriveAll0 ''SrcLoc)

instance Arbitrary CallStack where
# if MIN_VERSION_base(4,9,0)
    arbitrary = oneof [ pure EmptyCallStack
                      , PushCallStack <$> arbitrary <*> arbitrary <@> EmptyCallStack
                      , pure $ FreezeCallStack EmptyCallStack
                      ]
# else
    arbitrary = genericArbitrary
# endif

instance Arbitrary SrcLoc where
    arbitrary = genericArbitrary
#endif
