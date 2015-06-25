{-# LANGUAGE CPP              #-}

#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeOperators    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
#endif
{-|
Module:      Text.Show.Text.GHC.Generics
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Monomorphic 'Show' functions for generics-related data types.
This module only exports functions if the compiler supports generics
(on GHC, 7.2 or above).

/Since: 0.3/
-}
module Text.Show.Text.GHC.Generics (
#if __GLASGOW_HASKELL__ < 702
    ) where
#else
      showbU1
    , showbPar1PrecWith
    , showbRec1Prec
    , showbRec1PrecWith
    , showbK1PrecWith
    , showbM1Prec
    , showbM1PrecWith
    , showbSumTypePrec
    , showbSumTypePrecWith
    , showbProductTypePrec
    , showbProductTypePrecWith
    , showbCompFunctorsPrec
    , showbCompFunctorsPrecWith
    , showbFixityPrec
    , showbAssociativity
    , showbArityPrec
    ) where

import Data.Text.Lazy.Builder (Builder)

import GHC.Generics (U1(..), Par1, Rec1(..), K1(..),
                     M1(..), (:+:)(..), (:*:)(..), (:.:)(..),
                     Fixity, Associativity, Arity)

import Prelude hiding (Show)

import Text.Show.Text.Classes (Show(showb, showbPrec), Show1(..), Show2(..))
import Text.Show.Text.Data.Integral ()
import Text.Show.Text.TH.Internal (deriveShow, deriveShow1, mkShowbPrec,
                                   mkShowbPrecWith, mkShowbPrecWith2)

-- | Convert a 'U1' value to a 'Builder'.
-- This function is only available with @base-4.4.0.0@ or later.
--
-- /Since: 0.3/
showbU1 :: U1 p -> Builder
showbU1 = showb
{-# INLINE showbU1 #-}

-- | Convert a 'Par1' value to a 'Builder' with the given show function and precedence.
-- This function is only available with @base-4.4.0.0@ or later.
--
-- /Since: 1/
showbPar1PrecWith :: (Int -> p -> Builder) -> Int -> Par1 p -> Builder
showbPar1PrecWith = showbPrecWith
{-# INLINE showbPar1PrecWith #-}

-- | Convert a 'Rec1' value to a 'Builder' with the given precedence.
-- This function is only available with @base-4.4.0.0@ or later.
--
-- /Since: 0.3/
showbRec1Prec :: Show (f p) => Int -> Rec1 f p -> Builder
showbRec1Prec = showbPrec
{-# INLINE showbRec1Prec #-}

-- | Convert a 'Rec1' value to a 'Builder' with the given show function and precedence.
-- This function is only available with @base-4.4.0.0@ or later.
--
-- /Since: 1/
showbRec1PrecWith :: Show1 f => (Int -> p -> Builder) -> Int -> Rec1 f p -> Builder
showbRec1PrecWith = showbPrecWith
{-# INLINE showbRec1PrecWith #-}

-- | Convert a 'K1' value to a 'Builder' with the given show function and precedence.
-- This function is only available with @base-4.4.0.0@ or later.
--
-- /Since: 1/
showbK1PrecWith :: (Int -> c -> Builder) -> Int -> K1 i c p -> Builder
showbK1PrecWith sp = showbPrecWith2 sp undefined
{-# INLINE showbK1PrecWith #-}

-- | Convert an 'M1' value to a 'Builder' with the given precedence.
-- This function is only available with @base-4.4.0.0@ or later.
--
-- /Since: 0.3/
showbM1Prec :: Show (f p) => Int -> M1 i c f p -> Builder
showbM1Prec = showbPrec
{-# INLINE showbM1Prec #-}

-- | Convert an 'M1' value to a 'Builder' with the given show function and precedence.
-- This function is only available with @base-4.4.0.0@ or later.
--
-- /Since: 1/
showbM1PrecWith :: Show1 f => (Int -> p -> Builder) -> Int -> M1 i c f p -> Builder
showbM1PrecWith = showbPrecWith
{-# INLINE showbM1PrecWith #-}

-- | Convert a '(:+:)' value to a 'Builder' with the given precedence.
-- This function is only available with @base-4.4.0.0@ or later.
--
-- /Since: 0.3/
showbSumTypePrec :: (Show (f p), Show (g p)) => Int -> (f :+: g) p -> Builder
showbSumTypePrec = showbPrec
{-# INLINE showbSumTypePrec #-}

-- | Convert a '(:+:)' value to a 'Builder' with the given show function and precedence.
-- This function is only available with @base-4.4.0.0@ or later.
--
-- /Since: 1/
showbSumTypePrecWith :: (Show1 f, Show1 g) => (Int -> p -> Builder) -> Int -> (f :+: g) p -> Builder
showbSumTypePrecWith = showbPrecWith
{-# INLINE showbSumTypePrecWith #-}

-- | Convert a '(:*:)' value to a 'Builder' with the given precedence.
-- This function is only available with @base-4.4.0.0@ or later.
--
-- /Since: 0.3/
showbProductTypePrec :: (Show (f p), Show (g p)) => Int -> (f :*: g) p -> Builder
showbProductTypePrec = showbPrec
{-# INLINE showbProductTypePrec #-}

-- | Convert a '(:*:)' value to a 'Builder' with the given show function and precedence.
-- This function is only available with @base-4.4.0.0@ or later.
--
-- /Since: 1/
showbProductTypePrecWith :: (Show1 f, Show1 g) => (Int -> p -> Builder) -> Int -> (f :*: g) p -> Builder
showbProductTypePrecWith = showbPrecWith
{-# INLINE showbProductTypePrecWith #-}

-- | Convert a '(:.:)' value to a 'Builder' with the given precedence.
-- This function is only available with @base-4.4.0.0@ or later.
--
-- /Since: 0.3/
showbCompFunctorsPrec :: Show (f (g p)) => Int -> (f :.: g) p -> Builder
showbCompFunctorsPrec = showbPrec
{-# INLINE showbCompFunctorsPrec #-}

-- | Convert a '(:.:)' value to a 'Builder' with the given show function and precedence.
-- This function is only available with @base-4.4.0.0@ or later.
--
-- /Since: 1/
showbCompFunctorsPrecWith :: (Show1 f, Show1 g) => (Int -> p -> Builder) -> Int -> (f :.: g) p -> Builder
showbCompFunctorsPrecWith = showbPrecWith
{-# INLINE showbCompFunctorsPrecWith #-}

-- | Convert a 'Fixity' value to a 'Builder' with the given precedence.
-- This function is only available with @base-4.4.0.0@ or later.
--
-- /Since: 0.3/
showbFixityPrec :: Int -> Fixity -> Builder
showbFixityPrec = showbPrec
{-# INLINE showbFixityPrec #-}

-- | Convert an 'Associativity' value to a 'Builder'.
-- This function is only available with @base-4.4.0.0@ or later.
--
-- /Since: 0.3/
showbAssociativity :: Associativity -> Builder
showbAssociativity = showb
{-# INLINE showbAssociativity #-}

-- | Convert an 'Arity' value to a 'Builder' with the given precedence.
-- This function is only available with @base-4.4.0.0@ or later.
--
-- /Since: 0.3/
showbArityPrec :: Int -> Arity -> Builder
showbArityPrec = showbPrec
{-# INLINE showbArityPrec #-}

instance Show (U1 p) where
    showbPrec = showbPrecWith undefined
$(deriveShow1 ''U1)

$(deriveShow  ''Par1)
$(deriveShow1 ''Par1)

instance Show (f p) => Show (Rec1 f p) where
    showbPrec = $(mkShowbPrec ''Rec1)
$(deriveShow1 ''Rec1)

instance Show c => Show (K1 i c p) where
    showbPrec = showbPrecWith undefined
instance Show c => Show1 (K1 i c) where
    showbPrecWith = showbPrecWith2 showbPrec
instance Show2 (K1 i) where
    showbPrecWith2 = $(mkShowbPrecWith2 ''K1)

instance Show (f p) => Show (M1 i c f p) where
    showbPrec = $(mkShowbPrec ''M1)
instance Show1 f => Show1 (M1 i c f) where
    showbPrecWith = $(mkShowbPrecWith ''M1)

instance (Show (f p), Show (g p)) => Show ((f :+: g) p) where
    showbPrec = $(mkShowbPrec ''(:+:))
$(deriveShow1 ''(:+:))

instance (Show (f p), Show (g p)) => Show ((f :*: g) p) where
    showbPrec = $(mkShowbPrec ''(:*:))
$(deriveShow1 ''(:*:))

instance Show (f (g p)) => Show ((f :.: g) p) where
    showbPrec = $(mkShowbPrec ''(:.:))
$(deriveShow1 ''(:.:))

$(deriveShow ''Fixity)
$(deriveShow ''Associativity)
$(deriveShow ''Arity)
#endif
