{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}

#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE DeriveGeneric        #-}
#endif

#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE PolyKinds            #-}
#endif

#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE DeriveLift           #-}
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
      -- * Generic @show@ functions
      -- $generics

      -- ** Understanding a compiler error
      -- $generic_err
      genericShowt
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
    , ShowFunsB(..)
      -- ** Strict 'TS.Text'
    , GTextShowT(..)
    , GTextShowConT(..)
    , ShowFunsT(..)
      -- ** Lazy 'TL.Text'
    , GTextShowTL(..)
    , GTextShowConTL(..)
    , ShowFunsTL(..)
      -- ** Other internals
    , IsNullary(..)
    , ConType(..)
    , Zero
    , One
    ) where

import           Data.Data (Data, Typeable)
import           Data.Functor.Contravariant (Contravariant(..))
import           Data.Monoid.Compat ((<>))
import qualified Data.Text    as TS (Text, pack, singleton)
import qualified Data.Text.IO as TS (putStrLn, hPutStrLn)
import qualified Data.Text.Lazy    as TL (Text, pack, singleton)
import qualified Data.Text.Lazy.IO as TL (putStrLn, hPutStrLn)
import qualified Data.Text.Lazy.Builder as TB (fromString, singleton)
import           Data.Text.Lazy.Builder (Builder)

import           Generics.Deriving.Base
#if __GLASGOW_HASKELL__ < 702
import qualified Generics.Deriving.TH as Generics (deriveAll)
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

#include "inline.h"

{- $generics

'TextShow' instances can be easily defined for data types that are 'Generic' instances.
The easiest way to do this is to use the @DeriveGeneric@ extension.

@
&#123;-&#35; LANGUAGE DeriveGeneric &#35;-&#125;
import GHC.Generics
import TextShow
import TextShow.Generic

data D a = D a
  deriving (Generic, Generic1)

instance TextShow a => TextShow (D a) where
    showbPrec = 'genericShowbPrec'

instance TextShow1 D where
    liftShowbPrec = 'genericLiftShowbPrec'
@
-}

{- $generic_err

Suppose you intend to use 'genericShowbPrec' to define a 'TextShow' instance.

@
data Oops = Oops
    -- forgot to add \"deriving Generic\" here!

instance TextShow Oops where
    showbPrec = 'genericShowbPrec'
@

If you forget to add a @deriving 'Generic'@ clause to your data type, at
compile-time, you might get an error message that begins roughly as follows:

@
No instance for ('GTextShowB' 'Zero' (Rep Oops))
@

This error can be confusing, but don't let it intimidate you. The correct fix is
simply to add the missing \"@deriving 'Generic'@\" clause.

Similarly, if the compiler complains about not having an instance for @('GTextShowB'
'One' (Rep1 Oops1))@, add a \"@deriving 'Generic1'@\" clause.
-}

-- | A 'Generic' implementation of 'showt'.
--
-- /Since: 2/
genericShowt :: (Generic a, GTextShowT Zero (Rep a)) => a -> TS.Text
genericShowt = genericShowtPrec 0

-- | A 'Generic' implementation of 'showtl'.
--
-- /Since: 2/
genericShowtl :: (Generic a, GTextShowTL Zero (Rep a)) => a -> TL.Text
genericShowtl = genericShowtlPrec 0

-- | A 'Generic' implementation of 'showPrect'.
--
-- /Since: 2/
genericShowtPrec :: (Generic a, GTextShowT Zero (Rep a)) => Int -> a -> TS.Text
genericShowtPrec p = gShowtPrec NoShowFunsT p . from

-- | A 'Generic' implementation of 'showtlPrec'.
--
-- /Since: 2/
genericShowtlPrec :: (Generic a, GTextShowTL Zero (Rep a)) => Int -> a -> TL.Text
genericShowtlPrec p = gShowtlPrec NoShowFunsTL p . from

-- | A 'Generic' implementation of 'showtList'.
--
-- /Since: 2/
genericShowtList :: (Generic a, GTextShowT Zero (Rep a)) => [a] -> TS.Text
genericShowtList = showtListWith genericShowt

-- | A 'Generic' implementation of 'showtlList'.
--
-- /Since: 2/
genericShowtlList :: (Generic a, GTextShowTL Zero (Rep a)) => [a] -> TL.Text
genericShowtlList = showtlListWith genericShowtl

-- | A 'Generic' implementation of 'showb'.
--
-- /Since: 2/
genericShowb :: (Generic a, GTextShowB Zero (Rep a)) => a -> Builder
genericShowb = genericShowbPrec 0

-- | A 'Generic' implementation of 'showbPrec'.
--
-- /Since: 2/
genericShowbPrec :: (Generic a, GTextShowB Zero (Rep a)) => Int -> a -> Builder
genericShowbPrec p = gShowbPrec NoShowFunsB p . from

-- | A 'Generic' implementation of 'showbList'.
--
-- /Since: 2/
genericShowbList :: (Generic a, GTextShowB Zero (Rep a)) => [a] -> Builder
genericShowbList = showbListWith genericShowb

-- | A 'Generic' implementation of 'printT'.
--
-- /Since: 2/
genericPrintT :: (Generic a, GTextShowT Zero (Rep a)) => a -> IO ()
genericPrintT = TS.putStrLn . genericShowt

-- | A 'Generic' implementation of 'printTL'.
--
-- /Since: 2/
genericPrintTL :: (Generic a, GTextShowTL Zero (Rep a)) => a -> IO ()
genericPrintTL = TL.putStrLn . genericShowtl

-- | A 'Generic' implementation of 'hPrintT'.
--
-- /Since: 2/
genericHPrintT :: (Generic a, GTextShowT Zero (Rep a)) => Handle -> a -> IO ()
genericHPrintT h = TS.hPutStrLn h . genericShowt

-- | A 'Generic' implementation of 'hPrintTL'.
--
-- /Since: 2/
genericHPrintTL :: (Generic a, GTextShowTL Zero (Rep a)) => Handle -> a -> IO ()
genericHPrintTL h = TL.hPutStrLn h . genericShowtl

-- | A 'Generic1' implementation of 'genericLiftShowbPrec'.
--
-- /Since: 2/
genericLiftShowbPrec :: (Generic1 f, GTextShowB One (Rep1 f))
                     => (Int -> a -> Builder) -> ([a] -> Builder)
                     -> Int -> f a -> Builder
genericLiftShowbPrec sp sl p = gShowbPrec (Show1FunsB sp sl) p . from1

-- | A 'Generic'/'Generic1' implementation of 'showbPrec1'.
--
-- /Since: 2/
genericShowbPrec1 :: ( Generic a, Generic1 f
                     , GTextShowB Zero (Rep  a)
                     , GTextShowB One  (Rep1 f)
                     )
                  => Int -> f a -> Builder
genericShowbPrec1 = genericLiftShowbPrec genericShowbPrec genericShowbList

-------------------------------------------------------------------------------

-- | Whether a constructor is a record ('Rec'), a tuple ('Tup'), is prefix ('Pref'),
-- or infix ('Inf').
--
-- /Since: 2/
data ConType = Rec | Tup | Pref | Inf String
  deriving ( Data
           , Eq
           , Ord
           , Read
           , Show
           , Typeable
#if __GLASGOW_HASKELL__ >= 702
           , Generic
#endif
#if __GLASGOW_HASKELL__ >= 800
           , Lift
#endif
           )

-- | A type-level indicator that 'TextShow' is being derived generically.
--
-- /Since: 3.2/
data Zero

-- | A type-level indicator that 'TextShow1' is being derived generically.
--
-- /Since: 3.2/
data One

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
times, once for each Text/Builder variant. I suppose I could use TH instead to make this
look a little nicer, but I haven't attempted that.
-}

#if __GLASGOW_HASKELL__ >= 708
#define DERIVE_TYPEABLE(name) deriving instance Typeable name
#else
#define DERIVE_TYPEABLE(name)
#endif

#if __GLASGOW_HASKELL__ >= 711
#define HASH_FUNS(text_type,one_hash,two_hash,hash_prec,from_char,from_string) \
one_hash, two_hash :: text_type; \
hash_prec :: Int -> Int;         \
one_hash  = from_char '#';       \
two_hash  = from_string "##";    \
hash_prec = const 0
#else
#define HASH_FUNS(text_type,one_hash,two_hash,hash_prec,from_char,from_string) \
one_hash, two_hash :: text_type; \
hash_prec :: Int -> Int;         \
one_hash  = mempty;              \
two_hash  = mempty;              \
hash_prec = id
#endif

#define GTEXT_SHOW(text_type,show_funs,no_show_funs,show1_funs,one_hash,two_hash,hash_prec,gtext_show,gshow_prec,gtext_show_con,gshow_prec_con,show_prec,lift_show_prec,show_space,show_paren,show_list_with,from_char,from_string) \
{- | A 'show_funs' value either stores nothing (for 'TextShow') or it stores            \
the two function arguments that show occurrences of the type parameter (for             \
'TextShow1').                                                                           \
                                                                                        \
/Since: 3.4/                                                                           \
-};                                                                                     \
data show_funs arity a where {                                                          \
    no_show_funs :: show_funs Zero a                                                    \
  ; show1_funs   :: (Int -> a -> text_type) -> ([a] -> text_type) -> show_funs One a    \
 } deriving Typeable;                                                                   \
                                                                                        \
instance Contravariant (show_funs arity) where {                                        \
    contramap _ no_show_funs       = no_show_funs                                       \
  ; contramap f (show1_funs sp sl) = show1_funs (\p -> sp p . f) (sl . map f)           \
 };                                                                                     \
                                                                                        \
{- | Class of generic representation types that can be converted to                     \
a 'text_type'. The @arity@ type variable indicates which type class is                  \
used. @'gtext_show' 'Zero'@ indicates 'TextShow' behavior, and                          \
@'gtext_show' 'One'@ indicates 'TextShow1' behavior.                                    \
                                                                                        \
/Since: 3.4/                                                                           \
-};                                                                                     \
class gtext_show arity f where {                                                        \
    {- | This is used as the default generic implementation of 'show_prec' (if the      \
    @arity@ is 'Zero') or 'lift_show_prec' (if the @arity@ is 'One').                   \
    -}                                                                                  \
  ; gshow_prec :: show_funs arity a -> Int -> f a -> text_type                          \
 };                                                                                     \
                                                                                        \
DERIVE_TYPEABLE(gtext_show);                                                            \
                                                                                        \
instance gtext_show arity f => gtext_show arity (D1 d f) where {                        \
    gshow_prec sfs p (M1 x) = gshow_prec sfs p x                                        \
 };                                                                                     \
                                                                                        \
instance gtext_show Zero V1 where {                                                     \
    gshow_prec _ _ !_ = error "Void show_prec"                                          \
 };                                                                                     \
                                                                                        \
instance gtext_show One V1 where {                                                      \
    gshow_prec _ _ !_ = error "Void lift_show_prec"                                     \
 };                                                                                     \
                                                                                        \
instance (gtext_show arity f, gtext_show arity g) => gtext_show arity (f :+: g) where { \
    gshow_prec sfs p (L1 x) = gshow_prec sfs p x                                        \
  ; gshow_prec sfs p (R1 x) = gshow_prec sfs p x                                        \
 };                                                                                     \
                                                                                        \
instance (Constructor c, gtext_show_con arity f, IsNullary f)                           \
      => gtext_show arity (C1 c f) where {                                              \
    gshow_prec sfs p c@(M1 x) = case fixity of {                                        \
        Prefix -> show_paren ( p > appPrec                                              \
                               && not (isNullary x || conIsTuple c)                     \
                             ) $                                                        \
               (if conIsTuple c                                                         \
                   then mempty                                                          \
                   else let cn = conName c                                              \
                        in show_paren (isInfixDataCon cn) $ from_string cn)             \
            <> (if isNullary x || conIsTuple c                                          \
                   then mempty                                                          \
                   else from_char ' ')                                                  \
            <> showbBraces t (gshow_prec_con t sfs appPrec1 x)                          \
      ; Infix _ m -> show_paren (p > m) $ gshow_prec_con t sfs (m+1) x                  \
      }                                                                                 \
    where {                                                                             \
        fixity :: Fixity                                                                \
      ; fixity = conFixity c                                                            \
                                                                                        \
      ; t :: ConType                                                                    \
      ; t = if conIsRecord c                                                            \
            then Rec                                                                    \
            else case conIsTuple c of {                                                 \
                True  -> Tup                                                            \
              ; False -> case fixity of {                                               \
                    Prefix    -> Pref                                                   \
                  ; Infix _ _ -> Inf $ conName c                                        \
                };                                                                      \
              }                                                                         \
                                                                                        \
      ; showbBraces :: ConType -> text_type -> text_type                                \
      ; showbBraces Rec     b = from_char '{' <> b <> from_char '}'                     \
      ; showbBraces Tup     b = from_char '(' <> b <> from_char ')'                     \
      ; showbBraces Pref    b = b                                                       \
      ; showbBraces (Inf _) b = b                                                       \
                                                                                        \
      ; conIsTuple :: C1 c f p -> Bool                                                  \
      ; conIsTuple = isTupleString . conName                                            \
     };                                                                                 \
 };                                                                                     \
                                                                                        \
{- | Class of generic representation types for which the 'ConType' has been             \
determined. The @arity@ type variable indicates which type class is                     \
used. @'gtext_show_con' 'Zero'@ indicates 'TextShow' behavior, and                      \
@'gtext_show_con' 'One'@ indicates 'TextShow1' behavior.                                \
-};                                                                                     \
class gtext_show_con arity f where {                                                    \
    {- | Convert value of a specific 'ConType' to a 'text_type' with the given          \
    precedence.                                                                         \
    -}                                                                                  \
  ; gshow_prec_con :: ConType -> show_funs arity a -> Int -> f a -> text_type           \
 };                                                                                     \
                                                                                        \
DERIVE_TYPEABLE(gtext_show_con);                                                        \
                                                                                        \
instance gtext_show_con arity U1 where {                                                \
    gshow_prec_con _ _ _ U1 = mempty                                                    \
 };                                                                                     \
                                                                                        \
instance gtext_show_con One Par1 where {                                                \
    gshow_prec_con _ (show1_funs sp _) p (Par1 x) = sp p x                              \
 };                                                                                     \
                                                                                        \
instance TextShow c => gtext_show_con arity (K1 i c) where {                            \
    gshow_prec_con _ _ p (K1 x) = show_prec p x                                         \
 };                                                                                     \
                                                                                        \
instance TextShow1 f => gtext_show_con One (Rec1 f) where {                             \
    gshow_prec_con _ (show1_funs sp sl) p (Rec1 x) = lift_show_prec sp sl p x           \
 };                                                                                     \
                                                                                        \
instance (Selector s, gtext_show_con arity f) => gtext_show_con arity (S1 s f) where {  \
    gshow_prec_con t sfs p sel@(M1 x)                                                   \
      | selName sel == "" = gshow_prec_con t sfs p x                                    \
      | otherwise         = infixRec                                                    \
                            <> " = "                                                    \
                            <> gshow_prec_con t sfs 0 x                                 \
      where {                                                                           \
        infixRec :: text_type                                                           \
      ; infixRec | isSymVar selectorName                                                \
                 = from_char '(' <> from_string selectorName <> from_char ')'           \
                 | otherwise                                                            \
                 = from_string selectorName                                             \
                                                                                        \
      ; selectorName :: String                                                          \
      ; selectorName = selName sel                                                      \
      }                                                                                 \
 };                                                                                     \
                                                                                        \
instance (gtext_show_con arity f, gtext_show_con arity g)                               \
      => gtext_show_con arity (f :*: g) where {                                         \
    gshow_prec_con t@Rec sfs _ (a :*: b) =                                              \
           gshow_prec_con t sfs 0 a                                                     \
        <> ", "                                                                         \
        <> gshow_prec_con t sfs 0 b                                                     \
  ; gshow_prec_con t@(Inf o) sfs p (a :*: b) =                                          \
           gshow_prec_con t sfs p a                                                     \
        <> show_space                                                                   \
        <> infixOp                                                                      \
        <> show_space                                                                   \
        <> gshow_prec_con t sfs p b                                                     \
      where {                                                                           \
        infixOp :: text_type                                                            \
      ; infixOp = if isInfixDataCon o                                                   \
                     then from_string o                                                 \
                     else from_char '`' <> from_string o <> from_char '`'               \
      }                                                                                 \
  ; gshow_prec_con t@Tup sfs _ (a :*: b) =                                              \
           gshow_prec_con t sfs 0 a                                                     \
        <> from_char ','                                                                \
        <> gshow_prec_con t sfs 0 b                                                     \
  ; gshow_prec_con t@Pref sfs p (a :*: b) =                                             \
           gshow_prec_con t sfs p a                                                     \
        <> show_space                                                                   \
        <> gshow_prec_con t sfs p b                                                     \
 };                                                                                     \
                                                                                        \
instance (TextShow1 f, gtext_show_con One g) => gtext_show_con One (f :.: g) where {    \
    gshow_prec_con t sfs p (Comp1 x) =                                                  \
      let gspc = gshow_prec_con t sfs                                                   \
      in lift_show_prec gspc (show_list_with (gspc 0)) p x                              \
 };                                                                                     \
                                                                                        \
instance gtext_show_con arity UChar where {                                             \
    gshow_prec_con _ _ p (UChar c)   = show_prec (hash_prec p) (C# c) <> one_hash       \
 };                                                                                     \
                                                                                        \
instance gtext_show_con arity UDouble where {                                           \
    gshow_prec_con _ _ p (UDouble d) = show_prec (hash_prec p) (D# d) <> two_hash       \
 };                                                                                     \
                                                                                        \
instance gtext_show_con arity UFloat where {                                            \
    gshow_prec_con _ _ p (UFloat f)  = show_prec (hash_prec p) (F# f) <> one_hash       \
 };                                                                                     \
                                                                                        \
instance gtext_show_con arity UInt where {                                              \
    gshow_prec_con _ _ p (UInt i)    = show_prec (hash_prec p) (I# i) <> one_hash       \
 };                                                                                     \
                                                                                        \
instance gtext_show_con arity UWord where {                                             \
    gshow_prec_con _ _ p (UWord w)   = show_prec (hash_prec p) (W# w) <> two_hash       \
 };                                                                                     \
                                                                                        \
HASH_FUNS(text_type,one_hash,two_hash,hash_prec,from_char,from_string);

GTEXT_SHOW(Builder,ShowFunsB,NoShowFunsB,Show1FunsB,oneHashB,twoHashB,hashPrecB,GTextShowB,gShowbPrec,GTextShowConB,gShowbPrecCon,showbPrec,liftShowbPrec,showbSpace,showbParen,showbListWith,TB.singleton,TB.fromString)
GTEXT_SHOW(TS.Text,ShowFunsT,NoShowFunsT,Show1FunsT,oneHashT,twoHashT,hashPrecT,GTextShowT,gShowtPrec,GTextShowConT,gShowtPrecCon,showtPrec,liftShowtPrec,showtSpace,showtParen,showtListWith,TS.singleton,TS.pack)
GTEXT_SHOW(TL.Text,ShowFunsTL,NoShowFunsTL,Show1FunsTL,oneHashTL,twoHashTL,hashPrecTL,GTextShowTL,gShowtlPrec,GTextShowConTL,gShowtlPrecCon,showtlPrec,liftShowtlPrec,showtlSpace,showtlParen,showtlListWith,TL.singleton,TL.pack)

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

#if __GLASGOW_HASKELL__ < 702
$(Generics.deriveAll ''ConType)
#endif

#if __GLASGOW_HASKELL__ < 800
$(deriveLift ''ConType)
#endif
