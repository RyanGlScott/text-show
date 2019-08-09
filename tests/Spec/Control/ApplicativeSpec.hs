{-|
Module:      Spec.Control.ApplicativeSpec
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for data types in the "Control.Applicative" module.
-}
module Spec.Control.ApplicativeSpec (main, spec) where

import Control.Applicative (Const, ZipList)
import Control.Monad.Trans.Instances ()

import Data.Orphans ()
import Data.Proxy.Compat (Proxy(..))

import Generics.Deriving.Instances ()

import Spec.Utils (matchesTextShowSpec, matchesTextShow1Spec,
                   genericTextShowSpec, genericTextShow1Spec)

import Test.Hspec (Spec, describe, hspec, parallel)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "Const Int Int" $
        matchesTextShow1Spec (Proxy :: Proxy (Const Int Int))
    describe "ZipList Int" $ do
        let p :: Proxy (ZipList Int)
            p = Proxy
        matchesTextShowSpec  p
        genericTextShowSpec  p
        genericTextShow1Spec p
