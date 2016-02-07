{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module:      Instances.Data.List.NonEmpty
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'Arbitrary' instance for 'NonEmpty'.
-}
module Instances.Data.List.NonEmpty () where

import Data.Functor.Classes (Show1(..))
import Data.List.NonEmpty (NonEmpty(..))

import Prelude ()
import Prelude.Compat

import Test.QuickCheck (Arbitrary(..))

instance Arbitrary a => Arbitrary (NonEmpty a) where
    arbitrary = (:|) <$> arbitrary <*> arbitrary

#if MIN_VERSION_transformers(0,4,0) && !(MIN_VERSION_transformers(0,5,0))
instance Show1 NonEmpty where
    showsPrec1 = showsPrec
#else
instance Show1 NonEmpty where
    liftShowsPrec sp sl p (h :| t) = showParen (p > infixPrec) $
        sp (infixPrec+1) h . showString " :| " . liftShowsPrec sp sl (infixPrec+1) t
      where
        infixPrec :: Int
        infixPrec = 5
#endif
