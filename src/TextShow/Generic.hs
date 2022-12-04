{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE EmptyCase             #-}
{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}

#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE DeriveLift            #-}
#endif

#if __GLASGOW_HASKELL__ >= 806
{-# LANGUAGE QuantifiedConstraints #-}
#endif

{-|
Module:      TextShow.Generic
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Generic versions of 'TextShow' and 'TextShow1' class functions, as an alternative to
"TextShow.TH", which uses Template Haskell. Because there is no 'Generic2'
class, 'TextShow2' cannot be implemented generically.

This implementation is loosely based off of the @Generics.Deriving.Show@ module
from the @generic-deriving@ library.

/Since: 2/
-}
module TextShow.Generic (
      -- * Generic adapter newtypes
      FromGeneric(..)
    , FromGeneric1(..)

      -- * Generic @show@ functions
      -- $generics
    , genericShowt
    , genericShowtl
    , genericShowtPrec
    , genericShowtlPrec
    , genericShowtList
    , genericShowtlList
    , genericShowb
    , genericShowbPrec
    , genericShowbList
    , genericPrintT
    , genericPrintTL
    , genericHPrintT
    , genericHPrintTL
    , genericLiftShowbPrec
    , genericShowbPrec1
      -- * Internals
      -- ** 'Builder'
    , GTextShowB(..)
    , GTextShowConB(..)
    , GTextShowB1(..)
    , GTextShowConB1(..)
      -- ** Strict 'TS.Text'
    , GTextShowT(..)
    , GTextShowConT(..)
    , GTextShowT1(..)
    , GTextShowConT1(..)
      -- ** Lazy 'TL.Text'
    , GTextShowTL(..)
    , GTextShowConTL(..)
    , GTextShowTL1(..)
    , GTextShowConTL1(..)
      -- ** Other internals
    , IsNullary(..)
    , ConType(..)
    ) where

import           Data.Data (Data, Typeable)
import qualified Data.Text    as TS (Text, pack, singleton)
import qualified Data.Text.IO as TS (putStrLn, hPutStrLn)
import qualified Data.Text.Lazy    as TL (Text, pack, singleton)
import qualified Data.Text.Lazy.IO as TL (putStrLn, hPutStrLn)
import qualified Data.Text.Lazy.Builder as TB (fromString, singleton)
import           Data.Text.Lazy.Builder (Builder)

import           Generics.Deriving.Base
#if !defined(__LANGUAGE_DERIVE_GENERIC1__)
import qualified Generics.Deriving.TH as Generics
#endif

import           GHC.Exts (Char(C#), Double(D#), Float(F#), Int(I#), Word(W#))
import           GHC.Show (appPrec, appPrec1)

import           Language.Haskell.TH.Lift

import           Prelude ()
import           Prelude.Compat

import           System.IO (Handle)

import           TextShow.Classes (TextShow(..), TextShow1(..),
                                   showbListWith, showbParen, showbSpace,
                                   showtListWith, showtParen, showtSpace,
                                   showtlListWith, showtlParen, showtlSpace,
                                   liftShowtPrec, liftShowtlPrec)
import           TextShow.Instances ()
import           TextShow.TH.Internal (deriveTextShow)
import           TextShow.Utils (isInfixDataCon, isSymVar, isTupleString)

{- $generics

'TextShow' instances can be easily defined for data types that are 'Generic' instances.
If you are using GHC 8.6 or later, the easiest way to do this is to use the
@DerivingVia@ extension.

@
&#123;-&#35; LANGUAGE DeriveGeneric, DerivingVia &#35;-&#125;
import GHC.Generics
import TextShow
import TextShow.Generic

data D a = D a
  deriving ('Generic', 'Generic1')
  deriving 'TextShow'  via 'FromGeneric'  (D a)
  deriving 'TextShow1' via 'FromGeneric1' D
@

Or, if you are using a version of GHC older than 8.6, one can alternatively
define these instances like so:

@
instance 'TextShow' a => 'TextShow' (D a) where
    'showbPrec' = 'genericShowbPrec'

instance 'TextShow1' D where
    'liftShowbPrec' = 'genericLiftShowbPrec'
@
-}

-- | An adapter newtype, suitable for @DerivingVia@.
-- The 'TextShow' instance for 'FromGeneric' leverages a 'Generic'-based
-- default. That is,
--
-- @
-- 'showbPrec' p ('FromGeneric' x) = 'genericShowbPrec' p x
-- @
--
-- /Since: 3.7.4/
newtype FromGeneric a = FromGeneric { fromGeneric :: a }
  deriving ( Data
           , Eq
           , Foldable
           , Functor
           , Generic
           , Generic1
           , Ord
           , Read
           , Show
           , Traversable
           , Typeable
#if __GLASGOW_HASKELL__ >= 800
           , Lift
#endif
           )

-- | /Since: 3.7.4/
instance (Generic a, GTextShowB (Rep a ())) => TextShow (FromGeneric a) where
  showbPrec p = genericShowbPrec p . fromGeneric

-- | An adapter newtype, suitable for @DerivingVia@.
-- The 'TextShow1' instance for 'FromGeneric1' leverages a 'Generic1'-based
-- default. That is,
--
-- @
-- 'liftShowbPrec' sp sl p ('FromGeneric1' x) = 'genericLiftShowbPrec' sp sl p x
-- @
--
-- /Since: 3.7.4/
newtype FromGeneric1 f a = FromGeneric1 { fromGeneric1 :: f a }
  deriving ( Eq
           , Ord
           , Read
           , Show
           , Generic
#if defined(__LANGUAGE_DERIVE_GENERIC1__)
           , Generic1
#endif
#if __GLASGOW_HASKELL__ >= 800
           , Lift
#endif
           )

deriving instance Foldable    f => Foldable    (FromGeneric1 f)
deriving instance Functor     f => Functor     (FromGeneric1 f)
deriving instance Traversable f => Traversable (FromGeneric1 f)
deriving instance Typeable FromGeneric1
deriving instance ( Data (f a), Typeable f, Typeable a
                  ) => Data (FromGeneric1 f (a :: *))

-- | /Since: 3.10/
instance (Generic1 f, GTextShowB (Rep1 f a)) => TextShow (FromGeneric1 f a) where
  showbPrec p = gShowbPrec p . from1 . fromGeneric1

-- | /Since: 3.7.4/
instance ( Generic1 f
#if __GLASGOW_HASKELL__ >= 806 && __GLASGOW_HASKELL__ < 902
           -- Unfortunately, the quantified superclass for GTextShowB1 doesn't
           -- work on pre-9.2 versions of GHC when using a GTextShowB1 (Rep1 f)
           -- constraint directly, perhaps due to
           -- https://gitlab.haskell.org/ghc/ghc/-/issues/14860#note_454218.
           -- Fortunately, we can make GHC come to its senses by using an
           -- equality constraint.
         , g ~ Rep1 f, GTextShowB1 g
#else
         , GTextShowB1 (Rep1 f)
#endif
         ) => TextShow1 (FromGeneric1 f) where
  liftShowbPrec sp sl p = genericLiftShowbPrec sp sl p . fromGeneric1

-- | A 'Generic' implementation of 'showt'.
--
-- /Since: 2/
genericShowt :: (Generic a, GTextShowT (Rep a ())) => a -> TS.Text
genericShowt = genericShowtPrec 0

-- | A 'Generic' implementation of 'showtl'.
--
-- /Since: 2/
genericShowtl :: (Generic a, GTextShowTL (Rep a ())) => a -> TL.Text
genericShowtl = genericShowtlPrec 0

-- | A 'Generic' implementation of 'showPrect'.
--
-- /Since: 2/
genericShowtPrec :: (Generic a, GTextShowT (Rep a ())) => Int -> a -> TS.Text
genericShowtPrec p = gShowtPrec p . fromRepUnit

-- | A 'Generic' implementation of 'showtlPrec'.
--
-- /Since: 2/
genericShowtlPrec :: (Generic a, GTextShowTL (Rep a ())) => Int -> a -> TL.Text
genericShowtlPrec p = gShowtlPrec p . fromRepUnit

-- | A 'Generic' implementation of 'showtList'.
--
-- /Since: 2/
genericShowtList :: (Generic a, GTextShowT (Rep a ())) => [a] -> TS.Text
genericShowtList = showtListWith genericShowt

-- | A 'Generic' implementation of 'showtlList'.
--
-- /Since: 2/
genericShowtlList :: (Generic a, GTextShowTL (Rep a ())) => [a] -> TL.Text
genericShowtlList = showtlListWith genericShowtl

-- | A 'Generic' implementation of 'showb'.
--
-- /Since: 2/
genericShowb :: (Generic a, GTextShowB (Rep a ())) => a -> Builder
genericShowb = genericShowbPrec 0

-- | A 'Generic' implementation of 'showbPrec'.
--
-- /Since: 2/
genericShowbPrec :: (Generic a, GTextShowB (Rep a ())) => Int -> a -> Builder
genericShowbPrec p = gShowbPrec p . fromRepUnit

-- | A 'Generic' implementation of 'showbList'.
--
-- /Since: 2/
genericShowbList :: (Generic a, GTextShowB (Rep a ())) => [a] -> Builder
genericShowbList = showbListWith genericShowb

-- | A 'Generic' implementation of 'printT'.
--
-- /Since: 2/
genericPrintT :: (Generic a, GTextShowT (Rep a ())) => a -> IO ()
genericPrintT = TS.putStrLn . genericShowt

-- | A 'Generic' implementation of 'printTL'.
--
-- /Since: 2/
genericPrintTL :: (Generic a, GTextShowTL (Rep a ())) => a -> IO ()
genericPrintTL = TL.putStrLn . genericShowtl

-- | A 'Generic' implementation of 'hPrintT'.
--
-- /Since: 2/
genericHPrintT :: (Generic a, GTextShowT (Rep a ())) => Handle -> a -> IO ()
genericHPrintT h = TS.hPutStrLn h . genericShowt

-- | A 'Generic' implementation of 'hPrintTL'.
--
-- /Since: 2/
genericHPrintTL :: (Generic a, GTextShowTL (Rep a ())) => Handle -> a -> IO ()
genericHPrintTL h = TL.hPutStrLn h . genericShowtl

-- | A 'Generic1' implementation of 'genericLiftShowbPrec'.
--
-- /Since: 2/
genericLiftShowbPrec :: (Generic1 f, GTextShowB1 (Rep1 f))
                     => (Int -> a -> Builder) -> ([a] -> Builder)
                     -> Int -> f a -> Builder
genericLiftShowbPrec sp sl p = gLiftShowbPrec sp sl p . from1

-- | A 'Generic'/'Generic1' implementation of 'showbPrec1'.
--
-- /Since: 2/
genericShowbPrec1 :: ( Generic a, Generic1 f
                     , GTextShowB (Rep a ())
                     , GTextShowB1 (Rep1 f)
                     )
                  => Int -> f a -> Builder
genericShowbPrec1 = genericLiftShowbPrec genericShowbPrec genericShowbList

-- | A type-specialized version of 'from' used to assist type inference.
fromRepUnit :: Generic a => a -> Rep a ()
fromRepUnit = from

-------------------------------------------------------------------------------

-- | Whether a constructor is a record ('Rec'), a tuple ('Tup'), is prefix ('Pref'),
-- or infix ('Inf').
--
-- /Since: 2/
data ConType = Rec | Tup | Pref | Inf String
  deriving ( Data
           , Eq
           , Generic
           , Ord
           , Read
           , Show
           , Typeable
#if __GLASGOW_HASKELL__ >= 800
           , Lift
#endif
           )

{-
I'm not particularly proud of the code below. The issue is that we need to be able to
generically work over Builders, strict Text, and lazy Text. We could just work
generically over Builders only and then convert to Text after the fact, but that results
in a drastic slowdown for certain datatypes (see GH-21 for an example).

For the most part, the shared functionality could be abstracted with a subclass of
Monoid that supports fromString, fromChar, etc. But there's a very small chance that
the code below is ever going to inline properly, and the runtime cost of all those
dictinary lookups is likely to be as bad as converting to Text at the end, if not worse.

Therefore, I perform some ugly CPP hackery to copy-paste the generic functionality three
times, once for each Text/Builder variant. At some point, I should replace this with TH.
See #33.
-}

hashPrec :: Int -> Int
#if __GLASGOW_HASKELL__ >= 711
hashPrec = const 0
#else
hashPrec = id
#endif

#if __GLASGOW_HASKELL__ >= 711
#define HASH_FUNS(text_type,one_hash,two_hash,from_char,from_string) \
one_hash, two_hash :: text_type; \
one_hash  = from_char '#';       \
two_hash  = from_string "##";
#else
#define HASH_FUNS(text_type,one_hash,two_hash,from_char,from_string) \
one_hash, two_hash :: text_type; \
one_hash  = mempty;              \
two_hash  = mempty;
#endif

-- For some mysterious reason, attaching INLINE pragmas to things in this
-- module causes GHC 8.10's simplifier to absolutely explode in terms of
-- compile times. This also affects 9.0, 8.8, and older versions of GHC to
-- varying degrees, usually adding a couple of minutes or more to the overall
-- compile times.
--
-- We'd still like to include the INLINE pragmas on 9.2 or later, however, as
-- it delivers a modest but measurable performance boost in the benchmark suite.
-- As a compromise, we use CPP to only attach INLINE annotations on 9.2 or
-- later.
#if __GLASGOW_HASKELL__ >= 902
#define INLINE_GE_902(f) {-# INLINE f #-};
#else
#define INLINE_GE_902(f)
#endif

#if __GLASGOW_HASKELL__ >= 806
#define QUANTIFIED_SUPERCLASS(class_name,f) (forall a. TextShow a => class_name (f a)) =>
#else
#define QUANTIFIED_SUPERCLASS(class_name,f)
#endif

#define GTEXT_SHOW(text_type,show_funs,one_hash,two_hash,gtext_show,gtext_show1,gshow_prec,glift_show_prec,gtext_show_con,gtext_show_con1,gshow_prec_con,glift_show_prec_con,show_prec,lift_show_prec,show_space,show_paren,show_list,show_list_with,from_char,from_string,c1_show_prec,s1_show_prec,product_show_prec,u_char_show_prec,u_double_show_prec,u_float_show_prec,u_int_show_prec,u_word_show_prec) \
{- | Class of generic representation types that can be converted to a                   \
'text_type'.                                                                            \
                                                                                        \
/Since: 3.10/                                                                           \
-};                                                                                     \
class gtext_show a where {                                                              \
  ; gshow_prec :: Int -> a -> text_type                                                 \
};                                                                                      \
deriving instance Typeable gtext_show;                                                  \
                                                                                        \
instance gtext_show (f p) => gtext_show (D1 d f p) where {                              \
  ; gshow_prec p (M1 x) = gshow_prec p x                                                \
};                                                                                      \
                                                                                        \
instance gtext_show (V1 p) where {                                                      \
  ; gshow_prec _ x = case x of {}                                                       \
};                                                                                      \
                                                                                        \
instance (gtext_show (f p), gtext_show (g p))                                           \
      => gtext_show ((f :+: g) p) where {                                               \
  ; gshow_prec p (L1 x) = gshow_prec p x                                                \
  ; gshow_prec p (R1 x) = gshow_prec p x                                                \
};                                                                                      \
                                                                                        \
instance (Constructor c, gtext_show_con (f p), IsNullary f)                             \
    => gtext_show (C1 c f p) where {                                                    \
  gshow_prec = c1_show_prec gshow_prec_con                                              \
};                                                                                      \
                                                                                        \
{- | Class of generic representation types for which the 'ConType' has been             \
determined.                                                                             \
                                                                                        \
/Since: 3.10/                                                                           \
-};                                                                                     \
class gtext_show_con a where {                                                          \
  ; gshow_prec_con :: ConType -> Int -> a -> text_type                                  \
};                                                                                      \
deriving instance Typeable gtext_show_con;                                              \
                                                                                        \
instance gtext_show_con (U1 p) where {                                                  \
  ; gshow_prec_con _ _ U1 = mempty                                                      \
};                                                                                      \
                                                                                        \
instance TextShow p => gtext_show_con (Par1 p) where {                                  \
  ; gshow_prec_con _ p (Par1 x) = show_prec p x                                         \
};                                                                                      \
                                                                                        \
instance TextShow c => gtext_show_con (K1 i c p) where {                                \
  ; gshow_prec_con _ p (K1 x) = show_prec p x                                           \
};                                                                                      \
                                                                                        \
instance (TextShow1 f, TextShow p) => gtext_show_con (Rec1 f p) where {                 \
  ; gshow_prec_con _ p (Rec1 x) = lift_show_prec show_prec show_list p x                \
};                                                                                      \
                                                                                        \
instance (Selector s, gtext_show_con (f p)) => gtext_show_con (S1 s f p) where {        \
  ; gshow_prec_con t = s1_show_prec $ gshow_prec_con t                                  \
};                                                                                      \
                                                                                        \
instance (gtext_show_con (f p), gtext_show_con (g p))                                   \
      => gtext_show_con ((f :*: g) p) where {                                           \
  ; gshow_prec_con t = product_show_prec (gshow_prec_con t) (gshow_prec_con t) t        \
};                                                                                      \
                                                                                        \
instance (TextShow1 f, gtext_show_con (g p)) => gtext_show_con ((f :.: g) p) where {    \
  ; gshow_prec_con t p (Comp1 x) =                                                      \
      let gspc = gshow_prec_con t                                                       \
      in lift_show_prec gspc (show_list_with (gspc 0)) p x                              \
};                                                                                      \
                                                                                        \
instance gtext_show_con (UChar p) where {                                               \
  ; gshow_prec_con _ = u_char_show_prec show_prec                                       \
};                                                                                      \
                                                                                        \
instance gtext_show_con (UDouble p) where {                                             \
  ; gshow_prec_con _ = u_double_show_prec show_prec                                     \
};                                                                                      \
                                                                                        \
instance gtext_show_con (UFloat p) where {                                              \
  ; gshow_prec_con _ = u_float_show_prec show_prec                                      \
};                                                                                      \
                                                                                        \
instance gtext_show_con (UInt p) where {                                                \
  ; gshow_prec_con _ = u_int_show_prec show_prec                                        \
};                                                                                      \
                                                                                        \
instance gtext_show_con (UWord p) where {                                               \
  ; gshow_prec_con _ = u_word_show_prec show_prec                                       \
};                                                                                      \
                                                                                        \
{- | Class of generic representation types for unary type constructors that can         \
be converted to a 'text_type'.                                                          \
                                                                                        \
/Since: 3.10/                                                                           \
-};                                                                                     \
class QUANTIFIED_SUPERCLASS(gtext_show,f)                                               \
      gtext_show1 f where {                                                             \
  ; glift_show_prec :: (Int -> a -> text_type) -> ([a] -> text_type)                    \
                    -> Int -> f a -> text_type                                          \
};                                                                                      \
deriving instance Typeable gtext_show1;                                                 \
                                                                                        \
instance gtext_show1 f => gtext_show1 (D1 d f) where {                                  \
  ; glift_show_prec sp sl p (M1 x) = glift_show_prec sp sl p x                          \
};                                                                                      \
                                                                                        \
instance gtext_show1 V1 where {                                                         \
  ; glift_show_prec _ _ _ x = case x of {}                                              \
};                                                                                      \
                                                                                        \
instance (gtext_show1 f, gtext_show1 g) => gtext_show1 (f :+: g) where {                \
  ; glift_show_prec sp sl p (L1 x) = glift_show_prec sp sl p x                          \
  ; glift_show_prec sp sl p (R1 x) = glift_show_prec sp sl p x                          \
};                                                                                      \
                                                                                        \
instance (Constructor c, gtext_show_con1 f, IsNullary f)                                \
    => gtext_show1 (C1 c f) where {                                                     \
  ; glift_show_prec sp sl = c1_show_prec $ glift_show_prec_con sp sl                    \
};                                                                                      \
                                                                                        \
{- | Class of generic representation types for unary type constructors for which        \
the 'ConType' has been determined.                                                      \
                                                                                        \
/Since: 3.10/                                                                           \
-};                                                                                     \
class QUANTIFIED_SUPERCLASS(gtext_show_con,f)                                           \
      gtext_show_con1 f where {                                                         \
  ; glift_show_prec_con :: (Int -> a -> text_type) -> ([a] -> text_type)                \
                        -> ConType -> Int -> f a -> text_type                           \
};                                                                                      \
deriving instance Typeable gtext_show_con1;                                             \
                                                                                        \
instance gtext_show_con1 U1 where {                                                     \
  ; glift_show_prec_con _ _ _ _ U1 = mempty                                             \
};                                                                                      \
                                                                                        \
instance gtext_show_con1 Par1 where {                                                   \
  ; glift_show_prec_con sp _ _ p (Par1 x) = sp p x                                      \
};                                                                                      \
                                                                                        \
instance TextShow c => gtext_show_con1 (K1 i c) where {                                 \
  ; glift_show_prec_con _ _ _ p (K1 x) = show_prec p x                                  \
};                                                                                      \
                                                                                        \
instance TextShow1 f => gtext_show_con1 (Rec1 f) where {                                \
  ; glift_show_prec_con sp sl _ p (Rec1 x) = lift_show_prec sp sl p x                   \
};                                                                                      \
                                                                                        \
instance (Selector s, gtext_show_con1 f) => gtext_show_con1 (S1 s f) where {            \
  ; glift_show_prec_con sp sl t = s1_show_prec $ glift_show_prec_con sp sl t            \
};                                                                                      \
                                                                                        \
instance (gtext_show_con1 f, gtext_show_con1 g)                                         \
      => gtext_show_con1 (f :*: g) where {                                              \
  ; glift_show_prec_con sp sl t =                                                       \
      product_show_prec (glift_show_prec_con sp sl t) (glift_show_prec_con sp sl t) t   \
};                                                                                      \
                                                                                        \
instance (TextShow1 f, gtext_show_con1 g) => gtext_show_con1 (f :.: g) where {          \
  ; glift_show_prec_con sp sl t p (Comp1 x) =                                           \
      let gspc = glift_show_prec_con sp sl t                                            \
      in lift_show_prec gspc (show_list_with (gspc 0)) p x                              \
};                                                                                      \
                                                                                        \
instance gtext_show_con1 UChar where {                                                  \
  ; glift_show_prec_con _ _ _ = u_char_show_prec show_prec                              \
};                                                                                      \
                                                                                        \
instance gtext_show_con1 UDouble where {                                                \
  ; glift_show_prec_con _ _ _ = u_double_show_prec show_prec                            \
};                                                                                      \
                                                                                        \
instance gtext_show_con1 UFloat where {                                                 \
  ; glift_show_prec_con _ _ _ = u_float_show_prec show_prec                             \
};                                                                                      \
                                                                                        \
instance gtext_show_con1 UInt where {                                                   \
  ; glift_show_prec_con _ _ _ = u_int_show_prec show_prec                               \
};                                                                                      \
                                                                                        \
instance gtext_show_con1 UWord where {                                                  \
  ; glift_show_prec_con _ _ _ = u_word_show_prec show_prec                              \
};                                                                                      \
                                                                                        \
c1_show_prec :: forall c f p.                                                           \
                (Constructor c, IsNullary f)                                            \
             => (ConType -> Int -> f p -> text_type)                                    \
             -> Int -> C1 c f p -> text_type;                                           \
c1_show_prec sp p c@(M1 x) = case fixity of {                                           \
  ; Prefix -> show_paren ( p > appPrec                                                  \
                           && not (isNullary x || conIsTuple c)                         \
                         ) $                                                            \
           (if conIsTuple c                                                             \
               then mempty                                                              \
               else let cn = conName c                                                  \
                    in show_paren (isInfixDataCon cn) $ from_string cn)                 \
        <> (if isNullary x || conIsTuple c                                              \
               then mempty                                                              \
               else from_char ' ')                                                      \
        <> showBraces t (sp t appPrec1 x)                                               \
  ; Infix _ m -> show_paren (p > m) $ sp t (m+1) x                                      \
} where {                                                                               \
    ; fixity :: Fixity                                                                  \
    ; fixity = conFixity c                                                              \
                                                                                        \
    ; t :: ConType                                                                      \
    ; t = if conIsRecord c                                                              \
          then Rec                                                                      \
          else case conIsTuple c of {                                                   \
                 ; True  -> Tup                                                         \
                 ; False -> case fixity of {                                            \
                     ; Prefix    -> Pref                                                \
                     ; Infix _ _ -> Inf $ conName c                                     \
                     };                                                                 \
                 };                                                                     \
                                                                                        \
    ; showBraces :: ConType -> text_type -> text_type                                   \
    ; showBraces Rec     b = from_char '{' <> b <> from_char '}'                        \
    ; showBraces Tup     b = from_char '(' <> b <> from_char ')'                        \
    ; showBraces Pref    b = b                                                          \
    ; showBraces (Inf _) b = b                                                          \
                                                                                        \
    ; conIsTuple :: C1 c f p -> Bool                                                    \
    ; conIsTuple = isTupleString . conName                                              \
  };                                                                                    \
INLINE_GE_902(c1_show_prec)                                                            \
                                                                                        \
s1_show_prec :: Selector s                                                              \
             => (Int -> f p -> text_type)                                               \
             -> Int -> S1 s f p -> text_type;                                           \
s1_show_prec sp p sel@(M1 x)                                                            \
  | selName sel == "" = sp p x                                                          \
  | otherwise         = infixRec                                                        \
                        <> " = "                                                        \
                        <> sp 0 x                                                       \
  where {                                                                               \
    ; infixRec :: text_type                                                             \
    ; infixRec | isSymVar selectorName                                                  \
               = from_char '(' <> from_string selectorName <> from_char ')'             \
               | otherwise                                                              \
               = from_string selectorName                                               \
                                                                                        \
    ; selectorName :: String                                                            \
    ; selectorName = selName sel                                                        \
  };                                                                                    \
INLINE_GE_902(s1_show_prec)                                                            \
                                                                                        \
product_show_prec :: (Int -> f p -> text_type) -> (Int -> g p -> text_type)             \
                  -> ConType -> Int -> (f :*: g) p -> text_type;                        \
product_show_prec spf spg t p (a :*: b) =                                               \
  case t of {                                                                           \
    ; Rec ->                                                                            \
           spf 0 a                                                                      \
        <> ", "                                                                         \
        <> spg 0 b                                                                      \
    ; Inf o ->                                                                          \
           spf p a                                                                      \
        <> show_space                                                                   \
        <> infixOp o                                                                    \
        <> show_space                                                                   \
        <> spg p b                                                                      \
    ; Tup ->                                                                            \
           spf 0 a                                                                      \
        <> from_char ','                                                                \
        <> spg 0 b                                                                      \
    ; Pref ->                                                                           \
           spf p a                                                                      \
        <> show_space                                                                   \
        <> spg p b                                                                      \
  } where {                                                                             \
      ; infixOp :: String -> text_type                                                  \
      ; infixOp o = if isInfixDataCon o                                                 \
                       then from_string o                                               \
                       else from_char '`' <> from_string o <> from_char '`'             \
  };                                                                                    \
INLINE_GE_902(product_show_prec)                                                       \
                                                                                        \
u_char_show_prec :: (Int -> Char -> text_type) -> Int -> UChar p -> text_type;          \
u_char_show_prec sp p (UChar c) = sp (hashPrec p) (C# c) <> one_hash;                   \
INLINE_GE_902(u_char_show_prec)                                                        \
                                                                                        \
u_double_show_prec :: (Int -> Double -> text_type) -> Int -> UDouble p -> text_type;    \
u_double_show_prec sp p (UDouble d) = sp (hashPrec p) (D# d) <> two_hash;               \
INLINE_GE_902(u_double_show_prec)                                                      \
                                                                                        \
u_float_show_prec :: (Int -> Float -> text_type) -> Int -> UFloat p -> text_type;       \
u_float_show_prec sp p (UFloat f) = sp (hashPrec p) (F# f) <> one_hash;                 \
INLINE_GE_902(u_float_show_prec)                                                       \
                                                                                        \
u_int_show_prec :: (Int -> Int -> text_type) -> Int -> UInt p -> text_type;             \
u_int_show_prec sp p (UInt i) = sp (hashPrec p) (I# i) <> one_hash;                     \
INLINE_GE_902(u_int_show_prec)                                                         \
                                                                                        \
u_word_show_prec :: (Int -> Word -> text_type) -> Int -> UWord p -> text_type;          \
u_word_show_prec sp p (UWord w) = sp (hashPrec p) (W# w) <> two_hash;                   \
INLINE_GE_902(u_word_show_prec)                                                        \
                                                                                        \
HASH_FUNS(text_type,one_hash,two_hash,from_char,from_string);

GTEXT_SHOW(Builder,ShowFunsB,oneHashB,twoHashB,GTextShowB,GTextShowB1,gShowbPrec,gLiftShowbPrec,GTextShowConB,GTextShowConB1,gShowbPrecCon,gLiftShowbPrecCon,showbPrec,liftShowbPrec,showbSpace,showbParen,showbList,showbListWith,TB.singleton,TB.fromString,c1ShowbPrec,s1ShowbPrec,productShowbPrec,uCharShowbPrec,uDoubleShowbPrec,uFloatShowbPrec,uIntShowbPrec,uWordShowbPrec)
GTEXT_SHOW(TS.Text,ShowFunsT,oneHashT,twoHashT,GTextShowT,GTextShowT1,gShowtPrec,gLiftShowtPrec,GTextShowConT,GTextShowConT1,gShowtPrecCon,gLiftShowtPrecCon,showtPrec,liftShowtPrec,showtSpace,showtParen,showtList,showtListWith,TS.singleton,TS.pack,c1ShowtPrec,s1ShowtPrec,productShowtPrec,uCharShowtPrec,uDoubleShowtPrec,uFloatShowtPrec,uIntShowtPrec,uWordShowtPrec)
GTEXT_SHOW(TL.Text,ShowFunsTL,oneHashTL,twoHashTL,GTextShowTL,GTextShowTL1,gShowtlPrec,gLiftShowtlPrec,GTextShowConTL,GTextShowConTL1,gShowtlPrecCon,gLiftShowtlPrecCon,showtlPrec,liftShowtlPrec,showtlSpace,showtlParen,showtlList,showtlListWith,TL.singleton,TL.pack,c1ShowtlPrec,s1ShowtlPrec,productShowtlPrec,uCharShowtlPrec,uDoubleShowtlPrec,uFloatShowtlPrec,uIntShowtlPrec,uWordShowtlPrec)

-- | Class of generic representation types that represent a constructor with
-- zero or more fields.
class IsNullary f where
    -- Returns 'True' if the constructor has no fields.
    isNullary :: f a -> Bool

instance IsNullary U1 where
    isNullary _ = True

instance IsNullary Par1 where
    isNullary _ = False

instance IsNullary (K1 i c) where
    isNullary _ = False

instance IsNullary f => IsNullary (S1 s f) where
    isNullary (M1 x) = isNullary x

instance IsNullary (Rec1 f) where
    isNullary _ = False

instance IsNullary (f :*: g) where
    isNullary _ = False

instance IsNullary (f :.: g) where
    isNullary _ = False

instance IsNullary UChar where
    isNullary _ = False

instance IsNullary UDouble where
    isNullary _ = False

instance IsNullary UFloat where
    isNullary _ = False

instance IsNullary UInt where
    isNullary _ = False

instance IsNullary UWord where
    isNullary _ = False

-------------------------------------------------------------------------------

$(deriveTextShow ''ConType)

#if __GLASGOW_HASKELL__ < 800
$(deriveLift ''ConType)
$(deriveLift ''FromGeneric)

instance Lift (f a) => Lift (FromGeneric1 f a) where
    lift = $(makeLift ''FromGeneric1)
#endif

#if !defined(__LANGUAGE_DERIVE_GENERIC1__)
$(Generics.deriveMeta           ''FromGeneric1)
$(Generics.deriveRepresentable1 ''FromGeneric1)
#endif
