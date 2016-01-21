{-# LANGUAGE CPP                  #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE MagicHash            #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module:      TextShow.GHC.Generics
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Monomorphic 'TextShow' functions for generics-related data types.

/Since: 2/
-}
module TextShow.GHC.Generics (
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
#if !(MIN_VERSION_base(4,9,0))
    , showbArityPrec
#endif
    , showbUCharPrec
    , showbUDoublePrec
    , showbUFloatPrec
    , showbUIntPrec
    , showbUWordPrec
    ) where

import Data.Text.Lazy.Builder (Builder)

import Generics.Deriving.Base

import TextShow.Classes (TextShow(..), TextShow1(..), TextShow2(..))
import TextShow.Data.Char     ()
import TextShow.Data.Floating ()
import TextShow.Data.Integral ()
import TextShow.TH.Internal (deriveTextShow, deriveTextShow1, makeShowbPrec,
                             makeShowbPrecWith, makeShowbPrecWith2)

#if !(MIN_VERSION_template_haskell(2,7,0))
import Data.Monoid.Compat ((<>))
import Data.Text.Lazy.Builder (fromString, singleton)

import GHC.Exts (Char(C#), Double(D#), Float(F#), Int(I#), Word(W#))
import GHC.Show (appPrec)

import TextShow.Classes (showbParen)
#endif

-- | Convert a 'U1' value to a 'Builder'.
--
-- /Since: 2/
showbU1 :: U1 p -> Builder
showbU1 = showb
{-# INLINE showbU1 #-}

-- | Convert a 'Par1' value to a 'Builder' with the given show function and precedence.
--
-- /Since: 2/
showbPar1PrecWith :: (Int -> p -> Builder) -> Int -> Par1 p -> Builder
showbPar1PrecWith = showbPrecWith
{-# INLINE showbPar1PrecWith #-}

-- | Convert a 'Rec1' value to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbRec1Prec :: TextShow (f p) => Int -> Rec1 f p -> Builder
showbRec1Prec = showbPrec
{-# INLINE showbRec1Prec #-}

-- | Convert a 'Rec1' value to a 'Builder' with the given show function and precedence.
--
-- /Since: 2/
showbRec1PrecWith :: TextShow1 f => (Int -> p -> Builder) -> Int -> Rec1 f p -> Builder
showbRec1PrecWith = showbPrecWith
{-# INLINE showbRec1PrecWith #-}

-- | Convert a 'K1' value to a 'Builder' with the given show function and precedence.
--
-- /Since: 2/
showbK1PrecWith :: (Int -> c -> Builder) -> Int -> K1 i c p -> Builder
showbK1PrecWith sp = showbPrecWith2 sp undefined
{-# INLINE showbK1PrecWith #-}

-- | Convert an 'M1' value to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbM1Prec :: TextShow (f p) => Int -> M1 i c f p -> Builder
showbM1Prec = showbPrec
{-# INLINE showbM1Prec #-}

-- | Convert an 'M1' value to a 'Builder' with the given show function and precedence.
--
-- /Since: 2/
showbM1PrecWith :: TextShow1 f => (Int -> p -> Builder) -> Int -> M1 i c f p -> Builder
showbM1PrecWith = showbPrecWith
{-# INLINE showbM1PrecWith #-}

-- | Convert a '(:+:)' value to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbSumTypePrec :: (TextShow (f p), TextShow (g p)) => Int -> (f :+: g) p -> Builder
showbSumTypePrec = showbPrec
{-# INLINE showbSumTypePrec #-}

-- | Convert a '(:+:)' value to a 'Builder' with the given show function and precedence.
--
-- /Since: 2/
showbSumTypePrecWith :: (TextShow1 f, TextShow1 g) => (Int -> p -> Builder) -> Int -> (f :+: g) p -> Builder
showbSumTypePrecWith = showbPrecWith
{-# INLINE showbSumTypePrecWith #-}

-- | Convert a '(:*:)' value to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbProductTypePrec :: (TextShow (f p), TextShow (g p)) => Int -> (f :*: g) p -> Builder
showbProductTypePrec = showbPrec
{-# INLINE showbProductTypePrec #-}

-- | Convert a '(:*:)' value to a 'Builder' with the given show function and precedence.
--
-- /Since: 2/
showbProductTypePrecWith :: (TextShow1 f, TextShow1 g) => (Int -> p -> Builder) -> Int -> (f :*: g) p -> Builder
showbProductTypePrecWith = showbPrecWith
{-# INLINE showbProductTypePrecWith #-}

-- | Convert a '(:.:)' value to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbCompFunctorsPrec :: TextShow (f (g p)) => Int -> (f :.: g) p -> Builder
showbCompFunctorsPrec = showbPrec
{-# INLINE showbCompFunctorsPrec #-}

-- | Convert a '(:.:)' value to a 'Builder' with the given show function and precedence.
--
-- /Since: 2/
showbCompFunctorsPrecWith :: (TextShow1 f, TextShow1 g) => (Int -> p -> Builder) -> Int -> (f :.: g) p -> Builder
showbCompFunctorsPrecWith = showbPrecWith
{-# INLINE showbCompFunctorsPrecWith #-}

-- | Convert a 'Fixity' value to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbFixityPrec :: Int -> Fixity -> Builder
showbFixityPrec = showbPrec
{-# INLINE showbFixityPrec #-}

-- | Convert an 'Associativity' value to a 'Builder'.
--
-- /Since: 2/
showbAssociativity :: Associativity -> Builder
showbAssociativity = showb
{-# INLINE showbAssociativity #-}

#if !(MIN_VERSION_base(4,9,0))
-- | Convert an 'Arity' value to a 'Builder' with the given precedence.
-- This function is only available with @base-4.8@ or earlier.
--
-- /Since: 2/
showbArityPrec :: Int -> Arity -> Builder
showbArityPrec = showbPrec
{-# INLINE showbArityPrec #-}
#endif

-- | Convert a 'UChar' to a 'Builder' with the given precedence.
--
-- /Since: 2.1.2/
showbUCharPrec :: Int -> UChar p -> Builder
showbUCharPrec = showbPrec
{-# INLINE showbUCharPrec #-}

-- | Convert a 'UDouble' to a 'Builder' with the given precedence.
--
-- /Since: 2.1.2/
showbUDoublePrec :: Int -> UDouble p -> Builder
showbUDoublePrec = showbPrec
{-# INLINE showbUDoublePrec #-}

-- | Convert a 'UFloat' to a 'Builder' with the given precedence.
--
-- /Since: 2.1.2/
showbUFloatPrec :: Int -> UFloat p -> Builder
showbUFloatPrec = showbPrec
{-# INLINE showbUFloatPrec #-}

-- | Convert a 'UInt' to a 'Builder' with the given precedence.
--
-- /Since: 2.1.2/
showbUIntPrec :: Int -> UInt p -> Builder
showbUIntPrec = showbPrec
{-# INLINE showbUIntPrec #-}

-- | Convert a 'UWord' to a 'Builder' with the given precedence.
--
-- /Since: 2.1.2/
showbUWordPrec :: Int -> UWord p -> Builder
showbUWordPrec = showbPrec
{-# INLINE showbUWordPrec #-}

instance TextShow (U1 p) where
    showbPrec = showbPrecWith undefined
$(deriveTextShow1 ''U1)

$(deriveTextShow  ''Par1)
$(deriveTextShow1 ''Par1)

instance TextShow (f p) => TextShow (Rec1 f p) where
    showbPrec = $(makeShowbPrec ''Rec1)
$(deriveTextShow1 ''Rec1)

instance TextShow c => TextShow (K1 i c p) where
    showbPrec = showbPrecWith undefined
instance TextShow c => TextShow1 (K1 i c) where
    showbPrecWith = showbPrecWith2 showbPrec
instance TextShow2 (K1 i) where
    showbPrecWith2 = $(makeShowbPrecWith2 ''K1)

instance TextShow (f p) => TextShow (M1 i c f p) where
    showbPrec = $(makeShowbPrec ''M1)
instance TextShow1 f => TextShow1 (M1 i c f) where
    showbPrecWith = $(makeShowbPrecWith ''M1)

instance (TextShow (f p), TextShow (g p)) => TextShow ((f :+: g) p) where
    showbPrec = $(makeShowbPrec ''(:+:))
$(deriveTextShow1 ''(:+:))

instance (TextShow (f p), TextShow (g p)) => TextShow ((f :*: g) p) where
    showbPrec = $(makeShowbPrec ''(:*:))
$(deriveTextShow1 ''(:*:))

instance TextShow (f (g p)) => TextShow ((f :.: g) p) where
    showbPrec = $(makeShowbPrec ''(:.:))
$(deriveTextShow1 ''(:.:))

#if MIN_VERSION_template_haskell(2,7,0)
instance TextShow (UChar p) where
    showbPrec = $(makeShowbPrec 'UChar)
$(deriveTextShow1 'UChar)

instance TextShow (UDouble p) where
    showbPrec = $(makeShowbPrec 'UDouble)
$(deriveTextShow1 'UDouble)

instance TextShow (UFloat p) where
    showbPrec = $(makeShowbPrec 'UFloat)
$(deriveTextShow1 'UFloat)

instance TextShow (UInt p) where
    showbPrec = $(makeShowbPrec 'UInt)
$(deriveTextShow1 'UInt)

instance TextShow (UWord p) where
    showbPrec = $(makeShowbPrec 'UWord)
$(deriveTextShow1 'UWord)
#else
instance TextShow (UChar p) where
    showbPrec = showbPrecWith undefined
instance TextShow1 UChar where
    showbPrecWith _ p (UChar c) = showbParen (p > appPrec) $
           fromString "UChar "    <> singleton '{'
        <> fromString "uChar# = " <> showb (C# c)
        <> singleton '}'

instance TextShow (UDouble p) where
    showbPrec = showbPrecWith undefined
instance TextShow1 UDouble where
    showbPrecWith _ p (UDouble d) = showbParen (p > appPrec) $
           fromString "UDouble "    <> singleton '{'
        <> fromString "uDouble# = " <> showb (D# d)
        <> singleton '}'

instance TextShow (UFloat p) where
    showbPrec = showbPrecWith undefined
instance TextShow1 UFloat where
    showbPrecWith _ p (UFloat f) = showbParen (p > appPrec) $
           fromString "UFloat "    <> singleton '{'
        <> fromString "uFloat# = " <> showb (F# f)
        <> singleton '}'

instance TextShow (UInt p) where
    showbPrec = showbPrecWith undefined
instance TextShow1 UInt where
    showbPrecWith _ p (UInt i) = showbParen (p > appPrec) $
           fromString "UInt "    <> singleton '{'
        <> fromString "uInt# = " <> showb (I# i)
        <> singleton '}'

instance TextShow (UWord p) where
    showbPrec = showbPrecWith undefined
instance TextShow1 UWord where
    showbPrecWith _ p (UWord w) = showbParen (p > appPrec) $
           fromString "UWord "    <> singleton '{'
        <> fromString "uWord# = " <> showb (W# w)
        <> singleton '}'
#endif

$(deriveTextShow ''Fixity)
$(deriveTextShow ''Associativity)
#if !(MIN_VERSION_base(4,9,0))
$(deriveTextShow ''Arity)
#endif
