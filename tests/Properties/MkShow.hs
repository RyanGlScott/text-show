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

import qualified Data.Text      as TS (pack)
import qualified Data.Text.Lazy as TL (pack)
import           Data.Text.Lazy.Builder (fromString)

import           Derived

import           Instances.Derived ()

import           Test.Tasty.QuickCheck (NonZero)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

import           Text.Show.Text.TH

-- | Verifies 'mkShow' (and related functions) produce the same output as their
--   'String' counterparts. This uses a data type that is a 'Show' instance.
prop_mkShowIsInstance :: Int -> AllAtOnce Int Int Int Int -> Bool
prop_mkShowIsInstance p a =
       TS.pack    (show        a   ) == $(mkShow         ''AllAtOnce)   a
    && TL.pack    (show        a   ) == $(mkShowLazy     ''AllAtOnce)   a
    && TS.pack    (showsPrec p a "") == $(mkShowPrec     ''AllAtOnce) p a
    && TL.pack    (showsPrec p a "") == $(mkShowPrecLazy ''AllAtOnce) p a
    && fromString (show        a   ) == $(mkShowb        ''AllAtOnce)   a
    && fromString (showsPrec p a "") == $(mkShowbPrec    ''AllAtOnce) p a

-- | Verifies 'mkShow' (and related functions) produce the same output as their
--   'String' counterparts. This uses a data type that is not a 'Show' instance.
prop_mkShowIsNotInstance :: Int -> NonZero Int -> Bool
prop_mkShowIsNotInstance p a =
       TS.pack    (show        a   ) == $(mkShow         ''NonZero)   a
    && TL.pack    (show        a   ) == $(mkShowLazy     ''NonZero)   a
    && TS.pack    (showsPrec p a "") == $(mkShowPrec     ''NonZero) p a
    && TL.pack    (showsPrec p a "") == $(mkShowPrecLazy ''NonZero) p a
    && fromString (show        a   ) == $(mkShowb        ''NonZero)   a
    && fromString (showsPrec p a "") == $(mkShowbPrec    ''NonZero) p a

-- prop_mkPrintIsInstance
-- prop_mkPrintIsNotInstance

mkShowTests :: [TestTree]
mkShowTests =
    [ testGroup "mkShow and related functions"
        [ testProperty "$(mkShow ''AllAtOnce) (a Show instance)"    prop_mkShowIsInstance
        , testProperty "$(mkShow ''NonZero) (not a Show instance)"  prop_mkShowIsNotInstance
--         , testProperty "$(mkPrint ''AllAtOnce) (a Show instance)"   prop_mkPrintIsInstance
--         , testProperty "$(mkPrint ''NonZero) (not a Show instance)" prop_mkPrintIsNotInstance
        ]
    ]