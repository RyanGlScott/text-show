{-# LANGUAGE CPP #-}

{-|
Module:      Spec.GHC.StaticPtrSpec
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ test for 'StaticPtr'.
-}
module Spec.GHC.StaticPtrSpec (main, spec) where

import Instances.GHC.StaticPtr ()

import Prelude ()
import Prelude.Compat

import Test.Hspec (Spec, hspec, parallel)

#if MIN_VERSION_base(4,8,0)
import Data.Proxy.Compat (Proxy(..))
import GHC.StaticPtr (StaticPtrInfo)
import Spec.Utils (matchesTextShowSpec)
import Test.Hspec (describe)
#endif

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $
#if MIN_VERSION_base(4,8,0)
    describe "StaticPtrInfo" $
        matchesTextShowSpec (Proxy :: Proxy StaticPtrInfo)
#else
    pure ()
#endif
