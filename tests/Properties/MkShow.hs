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

@QuickCheck@ properties for 'mkShow' and related functions in "Text.Show.Text.TH".
-}
module Properties.MkShow (mkShowTests) where

import qualified Data.Text      as TS (Text, pack)
import qualified Data.Text.Lazy as TL (Text, pack)
import           Data.Text.Lazy.Builder (Builder, fromString)

import           Derived

import           Instances.Derived ()

import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (Arbitrary, testProperty)

import qualified Text.Show as S (Show)
import           Text.Show.Text.TH

-- | Verifies 'mkShow' (and related functions) produce the same output as their
-- 'String' counterparts.
prop_mkShow :: (Arbitrary a, S.Show a)
            => (a -> TS.Text)        -- ^ 'mkShow'
            -> (a -> TL.Text)        -- ^ 'mkShowLazy'
            -> (a -> Builder)        -- ^ 'mkShowb'
            -> (Int -> a -> TS.Text) -- ^ 'mkShowPrec'
            -> (Int -> a -> TL.Text) -- ^ 'mkShowPrecLazy'
            -> (Int -> a -> Builder) -- ^ 'mkShowbPrec'
            -> ([a] -> TS.Text)      -- ^ 'mkShowList'
            -> ([a] -> TL.Text)      -- ^ 'mkShowListLazy'
            -> ([a] -> Builder)      -- ^ 'mkShowbList'
            -> Int -> a -> Bool
prop_mkShow s1 s2 s3 sp1 sp2 sp3 sl1 sl2 sl3 p a =
       TS.pack    (show          a   ) == s1      a
    && TL.pack    (show          a   ) == s2      a
    && fromString (show          a   ) == s3      a
    && TS.pack    (showsPrec p   a "") == sp1 p   a
    && TL.pack    (showsPrec p   a "") == sp2 p   a
    && fromString (showsPrec p   a "") == sp3 p   a
    && TS.pack    (showList    [a] "") == sl1   [a]
    && TL.pack    (showList    [a] "") == sl2   [a]
    && fromString (showList    [a] "") == sl3   [a]

-- | Verifies 'mkShow' (and related functions) produce the same output as their
-- 'String' counterparts. This uses a plain type constructor.
prop_mkShowTyCon :: Int -> AllAtOnce Int Int Int Int -> Bool
prop_mkShowTyCon = prop_mkShow
    $(mkShow         ''AllAtOnce)
    $(mkShowLazy     ''AllAtOnce)
    $(mkShowb        ''AllAtOnce)
    $(mkShowPrec     ''AllAtOnce)
    $(mkShowPrecLazy ''AllAtOnce)
    $(mkShowbPrec    ''AllAtOnce)
    $(mkShowList     ''AllAtOnce)
    $(mkShowListLazy ''AllAtOnce)
    $(mkShowbList    ''AllAtOnce)

#if MIN_VERSION_template_haskell(2,7,0)
-- | Verifies 'mkShow' (and related functions) produce the same output as their
-- 'String' counterparts. This uses a data family name.
prop_mkShowDataFam :: Int -> OneDataInstance Int Int Int Int -> Bool
prop_mkShowDataFam = prop_mkShow
    $(mkShow         ''OneDataInstance)
    $(mkShowLazy     ''OneDataInstance)
    $(mkShowb        ''OneDataInstance)
    $(mkShowPrec     ''OneDataInstance)
    $(mkShowPrecLazy ''OneDataInstance)
    $(mkShowbPrec    ''OneDataInstance)
    $(mkShowList     ''OneDataInstance)
    $(mkShowListLazy ''OneDataInstance)
    $(mkShowbList    ''OneDataInstance)

-- | Verifies 'mkShow' (and related functions) produce the same output as their
-- 'String' counterparts. This uses a data family instance constructor.
prop_mkShowDataFamInstCon :: Int -> NotAllShow Int Int Int Int -> Bool
prop_mkShowDataFamInstCon = prop_mkShow
    $(mkShow         'NASShow1)
    $(mkShowLazy     'NASShow1)
    $(mkShowb        'NASShow1)
    $(mkShowPrec     'NASShow1)
    $(mkShowPrecLazy 'NASShow1)
    $(mkShowbPrec    'NASShow1)
    $(mkShowList     'NASShow1)
    $(mkShowListLazy 'NASShow1)
    $(mkShowbList    'NASShow1)
#endif

-- prop_mkPrint
-- prop_trace

mkShowTests :: [TestTree]
mkShowTests =
    [ testGroup "mkShow and related functions"
        [ testProperty "$(mkShow ''AllAtOnce) (a plain type constructor)"             prop_mkShowTyCon
--         , testProperty "$(mkPrint ''AllAtOnce) (a plain type constructor)"            prop_mkPrintTyCon
#if MIN_VERSION_template_haskell(2,7,0)
        , testProperty "$(mkShow ''NotAllShow) (a data family instance constructor)"  prop_mkShowDataFamInstCon
--         , testProperty "$(mkPrint ''NotAllShow) (a data family instance constructor)" prop_mkShowDataFamInstCon
        , testProperty "$(mkShow ''OneDataInstance) (a data family name)"             prop_mkShowDataFam
--         , testProperty "$(mkPrint ''OneDataInstance) (a data family name)"            prop_mkPrintDataFam
#endif
        ]
    ]