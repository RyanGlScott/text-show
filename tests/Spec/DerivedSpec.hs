{-# LANGUAGE CPP           #-}
{-# LANGUAGE MagicHash     #-}
{-# LANGUAGE TypeOperators #-}
{-|
Module:      Spec.DerivedSpec
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for data types that have derived 'Show' instances
(using "Text.Show.Text.TH").
-}
module Spec.DerivedSpec (main, spec) where

import Derived

import Instances.Derived ()

import Spec.Utils (prop_matchesShow)
#if __GLASGOW_HASKELL__ >= 702
import Spec.Utils (prop_genericShow)
#endif

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel . describe "Template Haskell-derived data types" $ do
    prop "Nullary instance"                                (prop_matchesShow :: Int -> Nullary -> Bool)
    prop "PhantomNullary Int instance"                     (prop_matchesShow :: Int -> PhantomNullary Int -> Bool)
    prop "MonomorphicUnary instance"                       (prop_matchesShow :: Int -> MonomorphicUnary -> Bool)
    prop "PolymorphicUnary Int Int instance"               (prop_matchesShow :: Int -> PolymorphicUnary Int Int -> Bool)
    prop "MonomorphicProduct instance"                     (prop_matchesShow :: Int -> MonomorphicProduct -> Bool)
    prop "PolymorphicProduct Int Int Int Int instance"     (prop_matchesShow :: Int -> PolymorphicProduct Int Int Int Int -> Bool)
    prop "MonomorphicRecord instance"                      (prop_matchesShow :: Int -> MonomorphicRecord -> Bool)
    prop "PolymorphicRecord Int Int Int Int instance"      (prop_matchesShow :: Int -> PolymorphicRecord Int Int Int Int -> Bool)
    prop "MonomorphicInfix instance"                       (prop_matchesShow :: Int -> MonomorphicInfix -> Bool)
    prop "PolymorphicInfix Int Int Int instance"           (prop_matchesShow :: Int -> PolymorphicInfix Int Int Int -> Bool)
    prop "MonomorphicForall instance"                      (prop_matchesShow :: Int -> MonomorphicForall -> Bool)
    prop "PolymorphicForall Int Int instance"              (prop_matchesShow :: Int -> PolymorphicForall Int Int -> Bool)
    prop "AllAtOnce Int Int Int Int instance"              (prop_matchesShow :: Int -> AllAtOnce Int Int Int Int -> Bool)
    prop "NormalGADT instance"                             (prop_matchesShow :: Int -> NormalGADT -> Bool)
    prop "ExistentialGADT Char Int Int instance"           (prop_matchesShow :: Int -> ExistentialGADT Char Int Int -> Bool)
    prop "ExistentialGADT Double Double Int instance"      (prop_matchesShow :: Int -> ExistentialGADT Double Double Int -> Bool)
    prop "ExistentialGADT Int String Int instance"         (prop_matchesShow :: Int -> ExistentialGADT Int String Int -> Bool)
    prop "ExistentialGADT Ordering Int Int instance"       (prop_matchesShow :: Int -> ExistentialGADT Ordering Int Int -> Bool)
    prop "ExistentialGADT Int Int Int instance"            (prop_matchesShow :: Int -> ExistentialGADT Int Int Int -> Bool)
    prop "PrimADT# Int instance"                           (prop_matchesShow :: Int -> PrimADT# Int -> Bool)
    prop "LeftAssocTree Int instance"                      (prop_matchesShow :: Int -> LeftAssocTree Int -> Bool)
    prop "RightAssocTree Int instance"                     (prop_matchesShow :: Int -> RightAssocTree Int -> Bool)
    prop "Int :?: Int instance"                            (prop_matchesShow :: Int -> Int :?: Int -> Bool)
    prop "HigherKindedTypeParams Maybe Int instance"       (prop_matchesShow :: Int -> HigherKindedTypeParams Maybe Int -> Bool)
    prop "RestrictedContext Int instance"                  (prop_matchesShow :: Int -> RestrictedContext Int -> Bool)
    prop "Fix Maybe instance"                              (prop_matchesShow :: Int -> Fix Maybe -> Bool)
#if MIN_VERSION_template_haskell(2,7,0)                                  
    prop "AllShow () () Int Int instance"                  (prop_matchesShow :: Int -> AllShow () () Int Int -> Bool)
    prop "AllShow Int Int Int Int instance"                (prop_matchesShow :: Int -> AllShow Int Int Int Int -> Bool)
    prop "AllShow Bool Bool Int Int instance"              (prop_matchesShow :: Int -> AllShow Bool Bool Int Int -> Bool)
    prop "AllShow Char Double Int Int instance"            (prop_matchesShow :: Int -> AllShow Char Double Int Int -> Bool)
    prop "AllShow Float Ordering Int Int instance"         (prop_matchesShow :: Int -> AllShow Float Ordering Int Int -> Bool)
    prop "NotAllShow Int Int Int Int instance"             (prop_matchesShow :: Int -> NotAllShow Int Int Int Int -> Bool)
    prop "AssocData1 () instance"                          (prop_matchesShow :: Int -> AssocData1 () -> Bool)
    prop "AssocData2 () instance"                          (prop_matchesShow :: Int -> AssocData2 () Int Int -> Bool)
# if __GLASGOW_HASKELL__ >= 708
    prop "NullaryData instance"                            (prop_matchesShow :: Int -> NullaryData -> Bool)
# endif
    prop "GADTFam Char Int Int instance"                   (prop_matchesShow :: Int -> GADTFam Char Int Int -> Bool)
    prop "GADTFam Double Double Int instance"              (prop_matchesShow :: Int -> GADTFam Double Double Int -> Bool)
    prop "GADTFam Int String Int instance"                 (prop_matchesShow :: Int -> GADTFam Int String Int -> Bool)
    prop "GADTFam Ordering Int Int instance"               (prop_matchesShow :: Int -> GADTFam Ordering Int Int -> Bool)
    prop "GADTFam Int Int Int instance"                    (prop_matchesShow :: Int -> GADTFam Int Int Int -> Bool)
#endif
#if __GLASGOW_HASKELL__ >= 702
    prop "Nullary generic show"                            (prop_genericShow :: Int -> Nullary -> Bool)
    prop "PhantomNullary Int generic show"                 (prop_genericShow :: Int -> PhantomNullary Int -> Bool)
    prop "MonomorphicUnary generic show"                   (prop_genericShow :: Int -> MonomorphicUnary -> Bool)
    prop "PolymorphicUnary Int Int generic show"           (prop_genericShow :: Int -> PolymorphicUnary Int Int -> Bool)
    prop "MonomorphicProduct generic show"                 (prop_genericShow :: Int -> MonomorphicProduct -> Bool)
    prop "PolymorphicProduct Int Int Int Int generic show" (prop_genericShow :: Int -> PolymorphicProduct Int Int Int Int -> Bool)
    prop "MonomorphicRecord generic show"                  (prop_genericShow :: Int -> MonomorphicRecord -> Bool)
    prop "PolymorphicRecord Int Int Int Int generic show"  (prop_genericShow :: Int -> PolymorphicRecord Int Int Int Int -> Bool)
    prop "MonomorphicInfix generic show"                   (prop_genericShow :: Int -> MonomorphicInfix -> Bool)
    prop "PolymorphicInfix Int Int Int generic show"       (prop_genericShow :: Int -> PolymorphicInfix Int Int Int -> Bool)
--     prop "MonomorphicForall generic show"                  (prop_genericShow :: Int -> MonomorphicForall -> Bool)
--     prop "PolymorphicForall Int Int generic show"          (prop_genericShow :: Int -> PolymorphicForall Int Int -> Bool)
--     prop "AllAtOnce Int Int Int Int generic show"          (prop_genericShow :: Int -> AllAtOnce Int Int Int Int -> Bool)
    prop "NormalGADT generic show"                         (prop_genericShow :: Int -> NormalGADT -> Bool)
--     prop "ExistentialGADT Char Int Int generic show"       (prop_genericShow :: Int -> GADT Char Int Int -> Bool)
--     prop "ExistentialGADT Double Double Int generic show"  (prop_genericShow :: Int -> GADT Double Double Int -> Bool)
--     prop "ExistentialGADT Int String Int generic show"     (prop_genericShow :: Int -> GADT Int String Int -> Bool)
--     prop "ExistentialGADT Ordering Int Int generic show"   (prop_genericShow :: Int -> GADT Ordering Int Int -> Bool)
--     prop "ExistentialGADT Int Int Int generic show"        (prop_genericShow :: Int -> GADT Int Int Int -> Bool)
    prop "LeftAssocTree Int generic show"                  (prop_genericShow :: Int -> LeftAssocTree Int -> Bool)
    prop "RightAssocTree Int generic show"                 (prop_genericShow :: Int -> RightAssocTree Int -> Bool)
    prop "Int :?: Int generic show"                        (prop_genericShow :: Int -> Int :?: Int -> Bool)
    prop "HigherKindedTypeParams Maybe Int generic show"   (prop_genericShow :: Int -> HigherKindedTypeParams Maybe Int -> Bool)
    prop "RestrictedContext Int generic show"              (prop_genericShow :: Int -> RestrictedContext Int -> Bool)
    prop "Fix Maybe generic show"                          (prop_genericShow :: Int -> Fix Maybe -> Bool)
# if MIN_VERSION_template_haskell(2,7,0) && __GLASGOW_HASKELL__ >= 706
    prop "AllShow () () Int Int generic show"              (prop_matchesShow :: Int -> AllShow () () Int Int -> Bool)
    prop "AllShow Int Int Int Int generic show"            (prop_matchesShow :: Int -> AllShow Int Int Int Int -> Bool)
    prop "AllShow Bool Bool Int Int generic show"          (prop_matchesShow :: Int -> AllShow Bool Bool Int Int -> Bool)
    prop "AllShow Char Double Int Int generic show"        (prop_matchesShow :: Int -> AllShow Char Double Int Int -> Bool)
    prop "AllShow Float Ordering Int Int generic show"     (prop_matchesShow :: Int -> AllShow Float Ordering Int Int -> Bool)
    prop "NotAllShow Int Int Int Int generic show"         (prop_genericShow :: Int -> NotAllShow Int Int Int Int -> Bool)
    prop "AssocData1 () generic show"                      (prop_genericShow :: Int -> AssocData1 () -> Bool)
    prop "AssocData2 () generic show"                      (prop_genericShow :: Int -> AssocData2 () Int Int -> Bool)
#  if __GLASGOW_HASKELL__ >= 708
    prop "NullaryData generic show"                        (prop_genericShow :: Int -> NullaryData -> Bool)
#  endif
# endif
#endif
