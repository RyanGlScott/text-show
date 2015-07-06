{-# LANGUAGE CPP #-}

{-|
Module:      Spec.GHC.EventSpec
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for data types in the "GHC.Event" module.
-}
module Spec.GHC.EventSpec (main, spec) where

import Instances.GHC.Event ()

import Prelude ()
import Prelude.Compat

import Test.Hspec (Spec, hspec, parallel)

#if !defined(__GHCJS__) && !defined(mingw32_HOST_OS) && MIN_VERSION_base(4,4,0)
import GHC.Event (Event)
# if MIN_VERSION_base(4,8,1)
import GHC.Event (Lifetime)
#endif

import Spec.Utils (prop_matchesTextShow)

import Test.Hspec (describe)
import Test.Hspec.QuickCheck (prop)
#endif


main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
#if !defined(__GHCJS__) && !defined(mingw32_HOST_OS) && MIN_VERSION_base(4,4,0)
    describe "Event" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> Event -> Bool)
--     describe "FdKey" $
--         prop "TextShow instance" (prop_matchesTextShow :: Int -> FdKey -> Bool)
# if MIN_VERSION_base(4,8,1)
    describe "Lifetime" $
        prop "TextShow instance" (prop_matchesTextShow :: Int -> Lifetime -> Bool)
# endif
#else
    pure ()
#endif
