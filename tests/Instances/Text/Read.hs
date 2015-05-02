{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module:      Instances.Text.Read
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

'Arbitrary' instances for data types in the "Text.Read" module.
-}
module Instances.Text.Read () where

import Prelude ()
import Prelude.Compat

import Test.QuickCheck (Arbitrary(..), oneof)

import Text.Read (Lexeme(..))

#if MIN_VERSION_base(4,7,0)
import Data.Fixed (Fixed, E12)
import Numeric (showEFloat, showFFloat, showGFloat, showHex, showOct)
import Test.QuickCheck (Gen, getNonNegative)
import Text.Read.Lex (Number)
#endif

instance Arbitrary Lexeme where
    arbitrary = oneof [ Char   <$> arbitrary
                      , String <$> arbitrary
                      , Punc   <$> arbitrary
                      , Ident  <$> arbitrary
                      , Symbol <$> arbitrary
#if MIN_VERSION_base(4,7,0)
                      , Number <$> arbitrary
#elif !(MIN_VERSION_base(4,6,0))
                      , Int    <$> arbitrary
                      , Rat    <$> arbitrary
#endif
                      , pure EOF
                      ]
    
#if MIN_VERSION_base(4,7,0)
instance Arbitrary Number where
    arbitrary = do
        str <- oneof [ show <$> (nonneg :: Gen Double)
                     , fmap (\d -> showEFloat Nothing d "") (nonneg :: Gen Double)
                     , fmap (\d -> showFFloat Nothing d "") (nonneg :: Gen Double)
                     , fmap (\d -> showGFloat Nothing d "") (nonneg :: Gen Double)
                     , show <$> (nonneg :: Gen Float)
                     , show <$> (nonneg :: Gen Int)
                     , fmap (\i -> "0x" ++ showHex i "") (nonneg :: Gen Int)
                     , fmap (\i -> "0o" ++ showOct i "") (nonneg :: Gen Int)
                     , show <$> (nonneg :: Gen Integer)
                     , show <$> (nonneg :: Gen Word)
                     , show <$> (nonneg :: Gen (Fixed E12))
                     ]
        let Number num = read str
        pure num
      where
        nonneg :: (Arbitrary a, Num a, Ord a) => Gen a
        nonneg = getNonNegative <$> arbitrary
#endif
