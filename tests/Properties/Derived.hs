{-# LANGUAGE CPP, TypeOperators #-}
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

import Properties.Utils (prop_matchesShow)
#if defined(GENERICS)
import Properties.Utils (prop_genericShow)
#endif

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Text.Show.Text (showb, showbPrec, FromStringShow(..))

-- | Verifies that the two 'Show' instances of 'GADT' coincide.
prop_showGADT :: Int    -- ^ The precedence to show with
              -> Double -- ^ The argument to 'GADTCon2'
              -> Int    -- ^ The argument to 'GADTCon3'
              -> Char   -- ^ The argument to 'GADTCon4'
              -> String -- ^ The argument to 'GADTCon5'
              -> Bool
prop_showGADT p d i c s
    = let gc1 :: GADT Char Int Int
          gc1 = GADTCon1
          
          gc2 :: GADT Double Double Int
          gc2 = GADTCon2 d
          
          gc3 :: GADT Int String Int
          gc3 = GADTCon3 i
          
          gc4 :: GADT Char String Int
          gc4 = GADTCon4 c
          
          gc5 :: GADT String String Int
          gc5 = GADTCon5 s
      in    showb       (FromStringShow gc1) == showb       gc1
         && showbPrec p (FromStringShow gc2) == showbPrec p gc2
         && showbPrec p (FromStringShow gc3) == showbPrec p gc3
         && showbPrec p (FromStringShow gc4) == showbPrec p gc4
         && showbPrec p (FromStringShow gc5) == showbPrec p gc5

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
        , testProperty "GADT instance"                                   prop_showGADT
        , testProperty "LeftAssocTree Int instance"                      (prop_matchesShow :: Int -> LeftAssocTree Int -> Bool)
        , testProperty "RightAssocTree Int instance"                     (prop_matchesShow :: Int -> RightAssocTree Int -> Bool)
        , testProperty "Int :?: Int instance"                            (prop_matchesShow :: Int -> Int :?: Int -> Bool)
        , testProperty "HigherKindedTypeParams Maybe Int instance"       (prop_matchesShow :: Int -> HigherKindedTypeParams Maybe Int -> Bool)
        , testProperty "RestrictedContext Int instance"                  (prop_matchesShow :: Int -> RestrictedContext Int -> Bool)
        , testProperty "Fix Maybe instance"                              (prop_matchesShow :: Int -> Fix Maybe -> Bool)
#if defined(GENERICS)
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
--         , testProperty "GADT generic show"                               prop_genericShowGADT
        , testProperty "LeftAssocTree Int generic show"                  (prop_genericShow :: Int -> LeftAssocTree Int -> Bool)
        , testProperty "RightAssocTree Int generic show"                 (prop_genericShow :: Int -> RightAssocTree Int -> Bool)
        , testProperty "Int :?: Int generic show"                        (prop_genericShow :: Int -> Int :?: Int -> Bool)
        , testProperty "HigherKindedTypeParams Maybe Int generic show"   (prop_genericShow :: Int -> HigherKindedTypeParams Maybe Int -> Bool)
        , testProperty "RestrictedContext Int generic show"              (prop_genericShow :: Int -> RestrictedContext Int -> Bool)
        , testProperty "Fix Maybe generic show"                          (prop_genericShow :: Int -> Fix Maybe -> Bool)
#endif
        ]
    ]