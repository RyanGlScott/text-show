{-# LANGUAGE CPP           #-}
{-# LANGUAGE TypeOperators #-}

{-|
Module:      Spec.Data.Type.EqualitySpec
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ test for '(:~:)'.
-}
module Spec.Data.Type.EqualitySpec (main, spec) where

import Instances.Data.Type.Equality ()

import Prelude ()
import Prelude.Compat

import Test.Hspec (Spec, hspec, parallel)

#if MIN_VERSION_base(4,7,0)
import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:))
import Spec.Utils (matchesTextShowSpec)
import Test.Hspec (describe)
#endif

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $
#if MIN_VERSION_base(4,7,0)
    describe "Int :~: Int" $
        matchesTextShowSpec (Proxy :: Proxy (Int :~: Int))
#else
    pure ()
#endif
