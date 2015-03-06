{-# LANGUAGE CPP, MagicHash, TypeOperators #-}
{-|
Module:      Properties.BaseAndFriends
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

@QuickCheck@ properties for data types that have derived 'Show' instances
(using "Text.Show.Text.TH").
-}
module Properties.Derived (derivedTests) where

import Derived

import Instances.Derived ()

import Properties.Utils (prop_matchesShow, prop_genericShow)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

derivedTests :: [TestTree]
derivedTests =
    [ testGroup "Template Haskell-derived data types"
        [ testProperty "Nullary instance"                                (prop_matchesShow :: Int -> Nullary -> Bool)
        , testProperty "PhantomNullary Int instance"                     (prop_matchesShow :: Int -> PhantomNullary Int -> Bool)
        , testProperty "MonomorphicUnary instance"                       (prop_matchesShow :: Int -> MonomorphicUnary -> Bool)
        , testProperty "PolymorphicUnary Int Int instance"               (prop_matchesShow :: Int -> PolymorphicUnary Int Int -> Bool)
        , testProperty "MonomorphicProduct instance"                     (prop_matchesShow :: Int -> MonomorphicProduct -> Bool)
        , testProperty "PolymorphicProduct Int Int Int Int instance"     (prop_matchesShow :: Int -> PolymorphicProduct Int Int Int Int -> Bool)
        , testProperty "MonomorphicRecord instance"                      (prop_matchesShow :: Int -> MonomorphicRecord -> Bool)
        , testProperty "PolymorphicRecord Int Int Int Int instance"      (prop_matchesShow :: Int -> PolymorphicRecord Int Int Int Int -> Bool)
        , testProperty "MonomorphicInfix instance"                       (prop_matchesShow :: Int -> MonomorphicInfix -> Bool)
        , testProperty "PolymorphicInfix Int Int Int instance"           (prop_matchesShow :: Int -> PolymorphicInfix Int Int Int -> Bool)
        , testProperty "MonomorphicForall instance"                      (prop_matchesShow :: Int -> MonomorphicForall -> Bool)
        , testProperty "PolymorphicForall Int Int instance"              (prop_matchesShow :: Int -> PolymorphicForall Int Int -> Bool)
        , testProperty "AllAtOnce Int Int Int Int instance"              (prop_matchesShow :: Int -> AllAtOnce Int Int Int Int -> Bool)
        , testProperty "GADT Char Int Int instance"                      (prop_matchesShow :: Int -> GADT Char Int Int -> Bool)
        , testProperty "GADT Double Double Int instance"                 (prop_matchesShow :: Int -> GADT Double Double Int -> Bool)
        , testProperty "GADT Int String Int instance"                    (prop_matchesShow :: Int -> GADT Int String Int -> Bool)
        , testProperty "GADT Ordering Int Int instance"                  (prop_matchesShow :: Int -> GADT Ordering Int Int -> Bool)
        , testProperty "GADT Int Int Int instance"                       (prop_matchesShow :: Int -> GADT Int Int Int -> Bool)
        , testProperty "PrimADT# Int instance"                           (prop_matchesShow :: Int -> PrimADT# Int -> Bool)
        , testProperty "LeftAssocTree Int instance"                      (prop_matchesShow :: Int -> LeftAssocTree Int -> Bool)
        , testProperty "RightAssocTree Int instance"                     (prop_matchesShow :: Int -> RightAssocTree Int -> Bool)
        , testProperty "Int :?: Int instance"                            (prop_matchesShow :: Int -> Int :?: Int -> Bool)
        , testProperty "HigherKindedTypeParams Maybe Int instance"       (prop_matchesShow :: Int -> HigherKindedTypeParams Maybe Int -> Bool)
        , testProperty "RestrictedContext Int instance"                  (prop_matchesShow :: Int -> RestrictedContext Int -> Bool)
        , testProperty "Fix Maybe instance"                              (prop_matchesShow :: Int -> Fix Maybe -> Bool)
#if MIN_VERSION_template_haskell(2,7,0)                                  
        , testProperty "AllShow () () Int Int instance"                  (prop_matchesShow :: Int -> AllShow () () Int Int -> Bool)
        , testProperty "AllShow Int Int Int Int instance"                (prop_matchesShow :: Int -> AllShow Int Int Int Int -> Bool)
        , testProperty "AllShow Bool Bool Int Int instance"              (prop_matchesShow :: Int -> AllShow Bool Bool Int Int -> Bool)
        , testProperty "AllShow Char Double Int Int instance"            (prop_matchesShow :: Int -> AllShow Char Double Int Int -> Bool)
        , testProperty "AllShow Float Ordering Int Int instance"         (prop_matchesShow :: Int -> AllShow Float Ordering Int Int -> Bool)
        , testProperty "NotAllShow Int Int Int Int instance"             (prop_matchesShow :: Int -> NotAllShow Int Int Int Int -> Bool)
        , testProperty "OneDataInstance Int Int Int Int instance"        (prop_matchesShow :: Int -> OneDataInstance Int Int Int Int -> Bool)
        , testProperty "AssocData1 () instance"                          (prop_matchesShow :: Int -> AssocData1 () -> Bool)
        , testProperty "AssocData2 () instance"                          (prop_matchesShow :: Int -> AssocData2 () Int Int -> Bool)
# if __GLASGOW_HASKELL__ >= 708 && __GLASGOW_HASKELL__ < 710
        , testProperty "NullaryData instance"                            (prop_matchesShow :: Int -> NullaryData -> Bool)
# endif
        , testProperty "GADTFam Char Int Int instance"                   (prop_matchesShow :: Int -> GADTFam Char Int Int -> Bool)
        , testProperty "GADTFam Double Double Int instance"              (prop_matchesShow :: Int -> GADTFam Double Double Int -> Bool)
        , testProperty "GADTFam Int String Int instance"                 (prop_matchesShow :: Int -> GADTFam Int String Int -> Bool)
        , testProperty "GADTFam Ordering Int Int instance"               (prop_matchesShow :: Int -> GADTFam Ordering Int Int -> Bool)
        , testProperty "GADTFam Int Int Int instance"                    (prop_matchesShow :: Int -> GADTFam Int Int Int -> Bool)
#endif
        , testProperty "Nullary generic show"                            (prop_genericShow :: Int -> Nullary -> Bool)
        , testProperty "PhantomNullary Int generic show"                 (prop_genericShow :: Int -> PhantomNullary Int -> Bool)
        , testProperty "MonomorphicUnary generic show"                   (prop_genericShow :: Int -> MonomorphicUnary -> Bool)
        , testProperty "PolymorphicUnary Int Int generic show"           (prop_genericShow :: Int -> PolymorphicUnary Int Int -> Bool)
        , testProperty "MonomorphicProduct generic show"                 (prop_genericShow :: Int -> MonomorphicProduct -> Bool)
        , testProperty "PolymorphicProduct Int Int Int Int generic show" (prop_genericShow :: Int -> PolymorphicProduct Int Int Int Int -> Bool)
        , testProperty "MonomorphicRecord generic show"                  (prop_genericShow :: Int -> MonomorphicRecord -> Bool)
        , testProperty "PolymorphicRecord Int Int Int Int generic show"  (prop_genericShow :: Int -> PolymorphicRecord Int Int Int Int -> Bool)
        , testProperty "MonomorphicInfix generic show"                   (prop_genericShow :: Int -> MonomorphicInfix -> Bool)
        , testProperty "PolymorphicInfix Int Int Int generic show"       (prop_genericShow :: Int -> PolymorphicInfix Int Int Int -> Bool)
--         , testProperty "MonomorphicForall generic show"                  (prop_genericShow :: Int -> MonomorphicForall -> Bool)
--         , testProperty "PolymorphicForall Int Int generic show"          (prop_genericShow :: Int -> PolymorphicForall Int Int -> Bool)
--         , testProperty "AllAtOnce Int Int Int Int generic show"          (prop_genericShow :: Int -> AllAtOnce Int Int Int Int -> Bool)
--         , testProperty "GADT Char Int Int generic show"                  (prop_genericShow :: Int -> GADT Char Int Int -> Bool)
--         , testProperty "GADT Double Double Int generic show"             (prop_genericShow :: Int -> GADT Double Double Int -> Bool)
--         , testProperty "GADT Int String Int generic show"                (prop_genericShow :: Int -> GADT Int String Int -> Bool)
--         , testProperty "GADT Ordering Int Int generic show"              (prop_genericShow :: Int -> GADT Ordering Int Int -> Bool)
--         , testProperty "GADT Int Int Int generic show"                   (prop_genericShow :: Int -> GADT Int Int Int -> Bool)
        , testProperty "LeftAssocTree Int generic show"                  (prop_genericShow :: Int -> LeftAssocTree Int -> Bool)
        , testProperty "RightAssocTree Int generic show"                 (prop_genericShow :: Int -> RightAssocTree Int -> Bool)
        , testProperty "Int :?: Int generic show"                        (prop_genericShow :: Int -> Int :?: Int -> Bool)
        , testProperty "HigherKindedTypeParams Maybe Int generic show"   (prop_genericShow :: Int -> HigherKindedTypeParams Maybe Int -> Bool)
        , testProperty "RestrictedContext Int generic show"              (prop_genericShow :: Int -> RestrictedContext Int -> Bool)
        , testProperty "Fix Maybe generic show"                          (prop_genericShow :: Int -> Fix Maybe -> Bool)
#if MIN_VERSION_template_haskell(2,7,0) && __GLASGOW_HASKELL__ >= 706
        , testProperty "AllShow () () Int Int generic show"              (prop_matchesShow :: Int -> AllShow () () Int Int -> Bool)
        , testProperty "AllShow Int Int Int Int generic show"            (prop_matchesShow :: Int -> AllShow Int Int Int Int -> Bool)
        , testProperty "AllShow Bool Bool Int Int generic show"          (prop_matchesShow :: Int -> AllShow Bool Bool Int Int -> Bool)
        , testProperty "AllShow Char Double Int Int generic show"        (prop_matchesShow :: Int -> AllShow Char Double Int Int -> Bool)
        , testProperty "AllShow Float Ordering Int Int generic show"     (prop_matchesShow :: Int -> AllShow Float Ordering Int Int -> Bool)
        , testProperty "NotAllShow Int Int Int Int generic show"         (prop_genericShow :: Int -> NotAllShow Int Int Int Int -> Bool)
        , testProperty "OneDataInstance Int Int Int Int generic show"    (prop_genericShow :: Int -> OneDataInstance Int Int Int Int -> Bool)
        , testProperty "AssocData1 () generic show"                      (prop_genericShow :: Int -> AssocData1 () -> Bool)
        , testProperty "AssocData2 () generic show"                      (prop_genericShow :: Int -> AssocData2 () Int Int -> Bool)
# if __GLASGOW_HASKELL__ >= 708
        , testProperty "NullaryData generic show"                        (prop_genericShow :: Int -> NullaryData -> Bool)
# endif
#endif
        ]
    ]
