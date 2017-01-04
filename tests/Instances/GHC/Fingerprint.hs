{-# LANGUAGE CPP                #-}

#if MIN_VERSION_base(4,4,0)
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
#endif

{-|
Module:      Instances.GHC.Fingerprint
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'Arbitrary' instance for 'Fingerprint'.
-}
module Instances.GHC.Fingerprint () where

#if MIN_VERSION_base(4,4,0)
import           GHC.Fingerprint.Type (Fingerprint(..))
# if __GLASGOW_HASKELL__ >= 704
import           GHC.Generics (Generic)
# else
import qualified Generics.Deriving.TH as Generics (deriveAll0)
# endif

import           Instances.Utils.GenericArbitrary (genericArbitrary)

import           Test.QuickCheck (Arbitrary(..))

instance Arbitrary Fingerprint where
    arbitrary = genericArbitrary

# if __GLASGOW_HASKELL__ >= 704
deriving instance Generic Fingerprint
# else
$(Generics.deriveAll0 ''Fingerprint)
# endif
#endif
