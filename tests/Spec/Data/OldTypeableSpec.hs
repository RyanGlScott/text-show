{-# LANGUAGE CPP #-}

#if MIN_VERSION_base(4,7,0) && !(MIN_VERSION_base(4,8,0))
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
#endif

{-|
Module:      Spec.Data.OldTypeableSpec
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for data types in the "Data.OldTypeable" module.
-}
module Spec.Data.OldTypeableSpec (main, spec) where

import Instances.Data.OldTypeable ()

import Prelude ()
import Prelude.Compat

import Test.Hspec (Spec, hspec, parallel)

#if MIN_VERSION_base(4,7,0) && !(MIN_VERSION_base(4,8,0))
import Data.OldTypeable (TyCon, TypeRep)
import Data.Proxy.Compat (Proxy(..))

import Spec.Utils (matchesTextShowSpec)

import Test.Hspec (describe)
#endif

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
#if MIN_VERSION_base(4,7,0) && !(MIN_VERSION_base(4,8,0))
    describe "TypeRep" $
        matchesTextShowSpec (Proxy :: Proxy TypeRep)
    describe "TyCon" $
        matchesTextShowSpec (Proxy :: Proxy TyCon)
#else
    pure ()
#endif
