{-# LANGUAGE CPP       #-}
{-# LANGUAGE MagicHash #-}

{-|
Module:      Spec.Derived.MagicHashSpec
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for data types with fields that have unlifted types.
-}
module Spec.Derived.MagicHashSpec (main, spec) where

import Data.Proxy (Proxy(..))
import Derived.MagicHash
import Spec.Utils (matchesTextShow1Spec, genericTextShowSpec, genericTextShow1Spec)
import Test.Hspec (Spec, describe, hspec, parallel)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "TyCon# Int Int" $ do
        let p :: Proxy (TyCon# Int Int)
            p = Proxy
        matchesTextShow1Spec p
        genericTextShowSpec  p
        genericTextShow1Spec p
#if MIN_VERSION_template_haskell(2,7,0)
    describe "TyFamily# Int Int" $ do
        let p :: Proxy (TyFamily# Int Int)
            p = Proxy
        matchesTextShow1Spec p
        genericTextShowSpec  p
        genericTextShow1Spec p
#endif
