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
    , liftShowbPar1Prec
    , showbRec1Prec
    , liftShowbRec1Prec
    , liftShowbK1Prec
    , showbM1Prec
    , liftShowbM1Prec
    , showbSumTypePrec
    , liftShowbSumTypePrec
    , showbProductTypePrec
    , liftShowbProductTypePrec
    , showbCompFunctorsPrec
    , liftShowbCompFunctorsPrec
    , showbFixityPrec
    , showbAssociativity
#if MIN_VERSION_base(4,9,0)
    , showbSourceUnpackedness
    , showbSourceStrictness
    , showbDecidedStrictness
#else
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
                             makeLiftShowbPrec, makeLiftShowbPrec2)

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
-- /Since: 3/
liftShowbPar1Prec :: (Int -> p -> Builder) -> Int -> Par1 p -> Builder
liftShowbPar1Prec sp = liftShowbPrec sp undefined
{-# INLINE liftShowbPar1Prec #-}

-- | Convert a 'Rec1' value to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbRec1Prec :: TextShow (f p) => Int -> Rec1 f p -> Builder
showbRec1Prec = showbPrec
{-# INLINE showbRec1Prec #-}

-- | Convert a 'Rec1' value to a 'Builder' with the given show function and precedence.
--
-- /Since: 3/
liftShowbRec1Prec :: TextShow1 f => (Int -> p -> Builder) -> ([p] -> Builder)
                  -> Int -> Rec1 f p -> Builder
liftShowbRec1Prec = liftShowbPrec
{-# INLINE liftShowbRec1Prec #-}

-- | Convert a 'K1' value to a 'Builder' with the given show function and precedence.
--
-- /Since: 3/
liftShowbK1Prec :: (Int -> c -> Builder) -> Int -> K1 i c p -> Builder
liftShowbK1Prec sp = liftShowbPrec2 sp undefined undefined undefined
{-# INLINE liftShowbK1Prec #-}

-- | Convert an 'M1' value to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbM1Prec :: TextShow (f p) => Int -> M1 i c f p -> Builder
showbM1Prec = showbPrec
{-# INLINE showbM1Prec #-}

-- | Convert an 'M1' value to a 'Builder' with the given show function and precedence.
--
-- /Since: 3/
liftShowbM1Prec :: TextShow1 f => (Int -> p -> Builder) -> ([p] -> Builder)
                -> Int -> M1 i c f p -> Builder
liftShowbM1Prec = liftShowbPrec
{-# INLINE liftShowbM1Prec #-}

-- | Convert a '(:+:)' value to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbSumTypePrec :: (TextShow (f p), TextShow (g p)) => Int -> (f :+: g) p -> Builder
showbSumTypePrec = showbPrec
{-# INLINE showbSumTypePrec #-}

-- | Convert a '(:+:)' value to a 'Builder' with the given show function and precedence.
--
-- /Since: 3/
liftShowbSumTypePrec :: (TextShow1 f, TextShow1 g)
                     => (Int -> p -> Builder) -> ([p] -> Builder)
                     -> Int -> (f :+: g) p -> Builder
liftShowbSumTypePrec = liftShowbPrec
{-# INLINE liftShowbSumTypePrec #-}

-- | Convert a '(:*:)' value to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbProductTypePrec :: (TextShow (f p), TextShow (g p)) => Int -> (f :*: g) p -> Builder
showbProductTypePrec = showbPrec
{-# INLINE showbProductTypePrec #-}

-- | Convert a '(:*:)' value to a 'Builder' with the given show function and precedence.
--
-- /Since: 3/
liftShowbProductTypePrec :: (TextShow1 f, TextShow1 g)
                         => (Int -> p -> Builder) -> ([p] -> Builder)
                         -> Int -> (f :*: g) p -> Builder
liftShowbProductTypePrec = liftShowbPrec
{-# INLINE liftShowbProductTypePrec #-}

-- | Convert a '(:.:)' value to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbCompFunctorsPrec :: TextShow (f (g p)) => Int -> (f :.: g) p -> Builder
showbCompFunctorsPrec = showbPrec
{-# INLINE showbCompFunctorsPrec #-}

-- | Convert a '(:.:)' value to a 'Builder' with the given show function and precedence.
--
-- /Since: 3/
liftShowbCompFunctorsPrec :: (TextShow1 f, TextShow1 g)
                          => (Int -> p -> Builder) -> ([p] -> Builder)
                          -> Int -> (f :.: g) p -> Builder
liftShowbCompFunctorsPrec = liftShowbPrec
{-# INLINE liftShowbCompFunctorsPrec #-}

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

#if MIN_VERSION_base(4,9,0)
-- | Convert a 'SourceUnpackedness' value to a 'Builder'.
-- This function is only available with @base-4.9.0.0@ or later.
--
-- /Since: 3/
showbSourceUnpackedness :: SourceUnpackedness -> Builder
showbSourceUnpackedness = showb
{-# INLINE showbSourceUnpackedness #-}

-- | Convert a 'SourceStrictness' value to a 'Builder'.
-- This function is only available with @base-4.9.0.0@ or later.
--
-- /Since: 3/
showbSourceStrictness :: SourceStrictness -> Builder
showbSourceStrictness = showb
{-# INLINE showbSourceStrictness #-}

-- | Convert a 'DecidedStrictness' value to a 'Builder'.
-- This function is only available with @base-4.9.0.0@ or later.
--
-- /Since: 3/
showbDecidedStrictness :: DecidedStrictness -> Builder
showbDecidedStrictness = showb
{-# INLINE showbDecidedStrictness #-}
#else
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
    showbPrec = liftShowbPrec undefined undefined
$(deriveTextShow1 ''U1)

$(deriveTextShow  ''Par1)
$(deriveTextShow1 ''Par1)

instance TextShow (f p) => TextShow (Rec1 f p) where
    showbPrec = $(makeShowbPrec ''Rec1)
$(deriveTextShow1 ''Rec1)

instance TextShow c => TextShow (K1 i c p) where
    showbPrec = liftShowbPrec undefined undefined
instance TextShow c => TextShow1 (K1 i c) where
    liftShowbPrec = liftShowbPrec2 showbPrec showbList
instance TextShow2 (K1 i) where
    liftShowbPrec2 = $(makeLiftShowbPrec2 ''K1)

instance TextShow (f p) => TextShow (M1 i c f p) where
    showbPrec = $(makeShowbPrec ''M1)
instance TextShow1 f => TextShow1 (M1 i c f) where
    liftShowbPrec = $(makeLiftShowbPrec ''M1)

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
    showbPrec = liftShowbPrec undefined undefined
instance TextShow1 UChar where
    liftShowbPrec _ _ p (UChar c) = showbParen (p > appPrec) $
           fromString "UChar "    <> singleton '{'
        <> fromString "uChar# = " <> showb (C# c)
        <> singleton '}'

instance TextShow (UDouble p) where
    showbPrec = liftShowbPrec undefined undefined
instance TextShow1 UDouble where
    liftShowbPrec _ _ p (UDouble d) = showbParen (p > appPrec) $
           fromString "UDouble "    <> singleton '{'
        <> fromString "uDouble# = " <> showb (D# d)
        <> singleton '}'

instance TextShow (UFloat p) where
    showbPrec = liftShowbPrec undefined undefined
instance TextShow1 UFloat where
    liftShowbPrec _ _ p (UFloat f) = showbParen (p > appPrec) $
           fromString "UFloat "    <> singleton '{'
        <> fromString "uFloat# = " <> showb (F# f)
        <> singleton '}'

instance TextShow (UInt p) where
    showbPrec = liftShowbPrec undefined undefined
instance TextShow1 UInt where
    liftShowbPrec _ _ p (UInt i) = showbParen (p > appPrec) $
           fromString "UInt "    <> singleton '{'
        <> fromString "uInt# = " <> showb (I# i)
        <> singleton '}'

instance TextShow (UWord p) where
    showbPrec = liftShowbPrec undefined undefined
instance TextShow1 UWord where
    liftShowbPrec _ _ p (UWord w) = showbParen (p > appPrec) $
           fromString "UWord "    <> singleton '{'
        <> fromString "uWord# = " <> showb (W# w)
        <> singleton '}'
#endif

$(deriveTextShow ''Fixity)
$(deriveTextShow ''Associativity)
#if MIN_VERSION_base(4,9,0)
$(deriveTextShow ''SourceUnpackedness)
$(deriveTextShow ''SourceStrictness)
$(deriveTextShow ''DecidedStrictness)
#else
$(deriveTextShow ''Arity)
#endif
