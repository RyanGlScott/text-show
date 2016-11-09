{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE PolyKinds       #-}
#endif

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module:      Instances.Data.Proxy
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'Arbitrary' instances for 'Proxy'.
-}
module Instances.Data.Proxy () where

import Data.Proxy (Proxy(..))
#if __GLASGOW_HASKELL__ < 702
import qualified Generics.Deriving.TH as Generics (deriveAll0)
#endif
import Test.QuickCheck (Arbitrary(..), arbitraryBoundedEnum)

instance Arbitrary (Proxy s) where
    arbitrary = arbitraryBoundedEnum

#if __GLASGOW_HASKELL__ < 702
$(Generics.deriveAll0 ''Proxy)
#endif
