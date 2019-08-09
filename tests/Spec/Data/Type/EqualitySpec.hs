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
import Data.Proxy.Compat (Proxy(..))
import Data.Type.Equality ((:~:))
# if MIN_VERSION_base(4,10,0)
import Data.Type.Equality ((:~~:))
# endif

import Spec.Utils (matchesTextShowSpec)
import Test.Hspec (describe)
#endif

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
#if MIN_VERSION_base(4,7,0)
    describe "Int :~: Int" $
        matchesTextShowSpec (Proxy :: Proxy (Int :~: Int))
# if MIN_VERSION_base(4,10,0)
    describe "Int :~~: Int" $
        matchesTextShowSpec (Proxy :: Proxy (Int :~~: Int))
# endif
#else
    pure ()
#endif
