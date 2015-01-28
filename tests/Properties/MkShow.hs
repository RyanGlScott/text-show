{-# LANGUAGE CPP, TemplateHaskell #-}
#if __GLASGOW_HASKELL__ < 706
-- Template Haskell's name generation didn't name-mangle very well prior to GHC
-- 7.6 and can cause name shadowing warnings, so suppress them.
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
#endif
{-|
Module:      Properties.MkShow
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

@QuickCheck@ properties for 'mkShowbPrec' in "Text.Show.Text.TH".
-}
module Properties.MkShow (mkShowTests) where

import           Derived (AllAtOnce)
#if MIN_VERSION_template_haskell(2,7,0)
import           Derived (NotAllShow(..), OneDataInstance)
#endif

import           Instances.Derived ()

import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (Arbitrary, testProperty)

import qualified Text.Show as S (Show)
import           Text.Show.Text (Builder, FromStringShow(..), showbPrec)
import           Text.Show.Text.TH (mkShowbPrec)

-- | Verifies 'mkShowbPrec' produces the same output as 'showsPrec' does.
prop_mkShowbPrec :: (Arbitrary a, S.Show a)
                 => (Int -> a -> Builder) -- ^ TH-generated 'mkShowbPrec' function
                 -> Int -> a -> Bool
prop_mkShowbPrec sf p x = showbPrec p (FromStringShow x) == sf p x

-- | Verifies 'mkShowbPrec' produces the same output as 'showsPrec' does.
-- This uses a plain type constructor.
prop_mkShowbPrecTyCon :: Int -> AllAtOnce Int Int Int Int -> Bool
prop_mkShowbPrecTyCon = prop_mkShowbPrec $(mkShowbPrec ''AllAtOnce)

#if MIN_VERSION_template_haskell(2,7,0)
-- | Verifies 'mkShowbPrec' produces the same output as 'showsPrec' does.
-- This uses a data family name.
prop_mkShowbPrecDataFam :: Int -> OneDataInstance Int Int Int Int -> Bool
prop_mkShowbPrecDataFam = prop_mkShowbPrec $(mkShowbPrec ''OneDataInstance)

-- | Verifies 'mkShowbPrec' produces the same output as 'showsPrec' does.
-- This uses a data family instance constructor.
prop_mkShowbPrecDataFamInstCon :: Int -> NotAllShow Int Int Int Int -> Bool
prop_mkShowbPrecDataFamInstCon = prop_mkShowbPrec $(mkShowbPrec 'NASShow1)
#endif

-- prop_mkPrint
-- prop_trace

mkShowTests :: [TestTree]
mkShowTests =
    [ testGroup "mkShow and related functions"
        [ testProperty "$(mkShowbPrec ''AllAtOnce) (a plain type constructor)"             prop_mkShowbPrecTyCon
--         , testProperty "$(mkPrint ''AllAtOnce) (a plain type constructor)"                 prop_mkPrintTyCon
#if MIN_VERSION_template_haskell(2,7,0)
        , testProperty "$(mkShowbPrec ''NotAllShow) (a data family instance constructor)"  prop_mkShowbPrecDataFamInstCon
--         , testProperty "$(mkPrint ''NotAllShow) (a data family instance constructor)"      prop_mkShowDataFamInstCon
        , testProperty "$(mkShowbPrec ''OneDataInstance) (a data family name)"             prop_mkShowbPrecDataFam
--         , testProperty "$(mkPrint ''OneDataInstance) (a data family name)"                 prop_mkPrintDataFam
#endif
        ]
    ]