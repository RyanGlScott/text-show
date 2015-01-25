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

import           Test.Tasty.QuickCheck (NonZero)
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
-- 'String' counterparts. This uses a data type that is a 'Show' instance.
prop_mkShowIsInstance :: Int -> AllAtOnce Int Int Int Int -> Bool
prop_mkShowIsInstance = prop_mkShow
    $(mkShow         ''AllAtOnce)
    $(mkShowLazy     ''AllAtOnce)
    $(mkShowb        ''AllAtOnce)
    $(mkShowPrec     ''AllAtOnce)
    $(mkShowPrecLazy ''AllAtOnce)
    $(mkShowbPrec    ''AllAtOnce)
    $(mkShowList     ''AllAtOnce)
    $(mkShowListLazy ''AllAtOnce)
    $(mkShowbList    ''AllAtOnce)

-- | Verifies 'mkShow' (and related functions) produce the same output as their
-- 'String' counterparts. This uses a data type that is not a 'Show' instance.
prop_mkShowIsNotInstance :: Int -> NonZero Int -> Bool
prop_mkShowIsNotInstance = prop_mkShow
    $(mkShow         ''NonZero)
    $(mkShowLazy     ''NonZero)
    $(mkShowb        ''NonZero)
    $(mkShowPrec     ''NonZero)
    $(mkShowPrecLazy ''NonZero)
    $(mkShowbPrec    ''NonZero)
    $(mkShowList     ''NonZero)
    $(mkShowListLazy ''NonZero)
    $(mkShowbList    ''NonZero)

-- prop_mkPrintIsInstance
-- prop_mkPrintIsNotInstance
-- prop_traceIsInstance
-- prop_traceIsNotInstance

mkShowTests :: [TestTree]
mkShowTests =
    [ testGroup "mkShow and related functions"
        [ testProperty "$(mkShow ''AllAtOnce) (a Show instance)"    prop_mkShowIsInstance
        , testProperty "$(mkShow ''NonZero) (not a Show instance)"  prop_mkShowIsNotInstance
--         , testProperty "$(mkPrint ''AllAtOnce) (a Show instance)"   prop_mkPrintIsInstance
--         , testProperty "$(mkPrint ''NonZero) (not a Show instance)" prop_mkPrintIsNotInstance
        ]
    ]