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
{-# LANGUAGE PolyKinds            #-}
#endif

#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE DeriveLift           #-}
#endif

{-|
Module:      TextShow.Generic
Copyright:   (C) 2014-2016 Ryan Scott
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
      -- * 'GTextShow' and friends
    , GTextShow(..)
    , GTextShowCon(..)
    , IsNullary(..)
    , ConType(..)
    , ShowFuns(..)
    , Zero
    , One
    ) where

import           Data.Functor.Contravariant (Contravariant(..))
import           Data.Monoid.Compat ((<>))
import qualified Data.Text    as TS (Text)
import qualified Data.Text.IO as TS (putStrLn, hPutStrLn)
import           Data.Text.Lazy (toStrict)
import           Data.Text.Lazy.Builder (Builder, fromString, singleton, toLazyText)
import qualified Data.Text.Lazy    as TL (Text)
import qualified Data.Text.Lazy.IO as TL (putStrLn, hPutStrLn)
import           Data.Typeable (Typeable)

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
                                   showbListWith, showbParen, showbSpace)
import           TextShow.Instances ()
import           TextShow.Utils (isInfixTypeCon, isTupleString)

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
No instance for ('GTextShow' 'Zero' (Rep Oops))
@

This error can be confusing, but don't let it intimidate you. The correct fix is
simply to add the missing \"@deriving 'Generic'@\" clause.

Similarly, if the compiler complains about not having an instance for @('GTextShow'
'One' (Rep1 Oops1))@, add a \"@deriving 'Generic1'@\" clause.
-}

-- | A 'Generic' implementation of 'showt'.
--
-- /Since: 2/
genericShowt :: (Generic a, GTextShow Zero (Rep a)) => a -> TS.Text
genericShowt = toStrict . genericShowtl

-- | A 'Generic' implementation of 'showtl'.
--
-- /Since: 2/
genericShowtl :: (Generic a, GTextShow Zero (Rep a)) => a -> TL.Text
genericShowtl = toLazyText . genericShowb

-- | A 'Generic' implementation of 'showPrect'.
--
-- /Since: 2/
genericShowtPrec :: (Generic a, GTextShow Zero (Rep a)) => Int -> a -> TS.Text
genericShowtPrec p = toStrict . genericShowtlPrec p

-- | A 'Generic' implementation of 'showtlPrec'.
--
-- /Since: 2/
genericShowtlPrec :: (Generic a, GTextShow Zero (Rep a)) => Int -> a -> TL.Text
genericShowtlPrec p = toLazyText . genericShowbPrec p

-- | A 'Generic' implementation of 'showtList'.
--
-- /Since: 2/
genericShowtList :: (Generic a, GTextShow Zero (Rep a)) => [a] -> TS.Text
genericShowtList = toStrict . genericShowtlList

-- | A 'Generic' implementation of 'showtlList'.
--
-- /Since: 2/
genericShowtlList :: (Generic a, GTextShow Zero (Rep a)) => [a] -> TL.Text
genericShowtlList = toLazyText . genericShowbList

-- | A 'Generic' implementation of 'showb'.
--
-- /Since: 2/
genericShowb :: (Generic a, GTextShow Zero (Rep a)) => a -> Builder
genericShowb = genericShowbPrec 0

-- | A 'Generic' implementation of 'showbPrec'.
--
-- /Since: 2/
genericShowbPrec :: (Generic a, GTextShow Zero (Rep a)) => Int -> a -> Builder
genericShowbPrec p = gShowbPrec NoShowFuns p . from

-- | A 'Generic' implementation of 'showbList'.
--
-- /Since: 2/
genericShowbList :: (Generic a, GTextShow Zero (Rep a)) => [a] -> Builder
genericShowbList = showbListWith genericShowb

-- | A 'Generic' implementation of 'printT'.
--
-- /Since: 2/
genericPrintT :: (Generic a, GTextShow Zero (Rep a)) => a -> IO ()
genericPrintT = TS.putStrLn . genericShowt

-- | A 'Generic' implementation of 'printTL'.
--
-- /Since: 2/
genericPrintTL :: (Generic a, GTextShow Zero (Rep a)) => a -> IO ()
genericPrintTL = TL.putStrLn . genericShowtl

-- | A 'Generic' implementation of 'hPrintT'.
--
-- /Since: 2/
genericHPrintT :: (Generic a, GTextShow Zero (Rep a)) => Handle -> a -> IO ()
genericHPrintT h = TS.hPutStrLn h . genericShowt

-- | A 'Generic' implementation of 'hPrintTL'.
--
-- /Since: 2/
genericHPrintTL :: (Generic a, GTextShow Zero (Rep a)) => Handle -> a -> IO ()
genericHPrintTL h = TL.hPutStrLn h . genericShowtl

-- | A 'Generic1' implementation of 'genericLiftShowbPrec'.
--
-- /Since: 2/
genericLiftShowbPrec :: (Generic1 f, GTextShow One (Rep1 f))
                     => (Int -> a -> Builder) -> ([a] -> Builder)
                     -> Int -> f a -> Builder
genericLiftShowbPrec sp sl p = gShowbPrec (Show1Funs sp sl) p . from1

-- | A 'Generic'/'Generic1' implementation of 'showbPrec1'.
--
-- /Since: 2/
genericShowbPrec1 :: ( Generic a, Generic1 f
                     , GTextShow Zero (Rep  a)
                     , GTextShow One  (Rep1 f)
                     )
                  => Int -> f a -> Builder
genericShowbPrec1 = genericLiftShowbPrec genericShowbPrec genericShowbList

-------------------------------------------------------------------------------

-- | Whether a constructor is a record ('Rec'), a tuple ('Tup'), is prefix ('Pref'),
-- or infix ('Inf').
--
-- /Since: 2/
data ConType = Rec | Tup | Pref | Inf String
  deriving ( Eq
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

instance TextShow ConType where
    showbPrec = genericShowbPrec
    INLINE_INST_FUN(showbPrec)

-- | A 'ShowFuns' value either stores nothing (for 'TextShow') or it stores
-- the two function arguments that show occurrences of the type parameter (for
-- 'TextShow1').
data ShowFuns arity a where
    NoShowFuns :: ShowFuns Zero a
    Show1Funs  :: (Int -> a -> Builder) -> ([a] -> Builder) -> ShowFuns One a
  deriving Typeable

instance Contravariant (ShowFuns arity) where
    contramap _ NoShowFuns        = NoShowFuns
    contramap f (Show1Funs sp sl) = Show1Funs (\p -> sp p . f) (sl . map f)

-- | A type-level indicator that 'TextShow' is being derived generically.
--
-- / Since: 3.2/
data Zero

-- | A type-level indicator that 'TextShow1' is being derived generically.
--
-- / Since: 3.2/
data One

-- | Class of generic representation types that can be converted to
-- a 'Builder'. The @arity@ type variable indicates which type class is
-- used. @'GTextShow' 'Zero'@ indicates 'TextShow' behavior, and
-- @'GTextShow' 'One'@ indicates 'TextShow1' behavior.
--
-- /Since: 3.2/
class GTextShow arity f where
    -- | This is used as the default generic implementation of 'showbPrec' (if the
    -- @arity@ is 'Zero') or 'liftShowbPrec' (if the @arity@ is 'One').
    gShowbPrec :: ShowFuns arity a -> Int -> f a -> Builder

#if __GLASGOW_HASKELL__ >= 708
deriving instance Typeable GTextShow
#endif

instance GTextShow arity f => GTextShow arity (D1 d f) where
    gShowbPrec sfs p (M1 x) = gShowbPrec sfs p x

instance GTextShow Zero V1 where
    gShowbPrec _ _ !_ = error "Void showbPrec"

instance GTextShow One V1 where
    gShowbPrec _ _ !_ = error "Void liftShowbPrec"

instance (GTextShow arity f, GTextShow arity g) => GTextShow arity (f :+: g) where
    gShowbPrec sfs p (L1 x) = gShowbPrec sfs p x
    gShowbPrec sfs p (R1 x) = gShowbPrec sfs p x

instance (Constructor c, GTextShowCon arity f, IsNullary f)
      => GTextShow arity (C1 c f) where
    gShowbPrec sfs p c@(M1 x) = case fixity of
        Prefix -> showbParen ( p > appPrec
                               && not (isNullary x || conIsTuple c)
                             ) $
               (if conIsTuple c
                   then mempty
                   else let cn = conName c
                        in showbParen (isInfixTypeCon cn) $ fromString cn)
            <> (if isNullary x || conIsTuple c
                   then mempty
                   else singleton ' ')
            <> showbBraces t (gShowbPrecCon t sfs appPrec1 x)
        Infix _ m -> showbParen (p > m) $ gShowbPrecCon t sfs (m+1) x
      where
        fixity :: Fixity
        fixity = conFixity c

        t :: ConType
        t = if conIsRecord c
            then Rec
            else case conIsTuple c of
                True  -> Tup
                False -> case fixity of
                    Prefix    -> Pref
                    Infix _ _ -> Inf $ conName c

        showbBraces :: ConType -> Builder -> Builder
        showbBraces Rec     b = singleton '{' <> b <> singleton '}'
        showbBraces Tup     b = singleton '(' <> b <> singleton ')'
        showbBraces Pref    b = b
        showbBraces (Inf _) b = b

        conIsTuple :: C1 c f p -> Bool
        conIsTuple = isTupleString . conName

-- | Class of generic representation types for which the 'ConType' has been
-- determined. The @arity@ type variable indicates which type class is
-- used. @'GTextShow' 'Zero'@ indicates 'TextShow' behavior, and
-- @'GTextShow' 'One'@ indicates 'TextShow1' behavior.
class GTextShowCon arity f where
    -- | Convert value of a specific 'ConType' to a 'Builder' with the given
    -- precedence.
    gShowbPrecCon :: ConType -> ShowFuns arity a -> Int -> f a -> Builder

#if __GLASGOW_HASKELL__ >= 708
deriving instance Typeable GTextShowCon
#endif

instance GTextShowCon arity U1 where
    gShowbPrecCon _ _ _ U1 = mempty

instance GTextShowCon One Par1 where
    gShowbPrecCon _ (Show1Funs sp _) p (Par1 x) = sp p x

instance TextShow c => GTextShowCon arity (K1 i c) where
    gShowbPrecCon _ _ p (K1 x) = showbPrec p x

instance TextShow1 f => GTextShowCon One (Rec1 f) where
    gShowbPrecCon _ (Show1Funs sp sl) p (Rec1 x) = liftShowbPrec sp sl p x

instance (Selector s, GTextShowCon arity f) => GTextShowCon arity (S1 s f) where
    gShowbPrecCon t sfs p sel@(M1 x)
      | selName sel == "" = gShowbPrecCon t sfs p x
      | otherwise         = fromString (selName sel)
                            <> " = "
                            <> gShowbPrecCon t sfs 0 x

instance (GTextShowCon arity f, GTextShowCon arity g)
      => GTextShowCon arity (f :*: g) where
    gShowbPrecCon t@Rec sfs _ (a :*: b) =
           gShowbPrecCon t sfs 0 a
        <> ", "
        <> gShowbPrecCon t sfs 0 b
    gShowbPrecCon t@(Inf o) sfs p (a :*: b) =
           gShowbPrecCon t sfs p a
        <> showbSpace
        <> infixOp
        <> showbSpace
        <> gShowbPrecCon t sfs p b
      where
        infixOp :: Builder
        infixOp = if isInfixTypeCon o
                     then fromString o
                     else singleton '`' <> fromString o <> singleton '`'
    gShowbPrecCon t@Tup sfs _ (a :*: b) =
           gShowbPrecCon t sfs 0 a
        <> singleton ','
        <> gShowbPrecCon t sfs 0 b
    gShowbPrecCon t@Pref sfs p (a :*: b) =
           gShowbPrecCon t sfs p a
        <> showbSpace
        <> gShowbPrecCon t sfs p b

instance (TextShow1 f, GTextShowCon One g) => GTextShowCon One (f :.: g) where
    gShowbPrecCon t sfs p (Comp1 x) =
      let gspc = gShowbPrecCon t sfs
      in liftShowbPrec gspc (showbListWith (gspc 0)) p x

instance GTextShowCon arity UChar where
    gShowbPrecCon _ _ p (UChar c)   = showbPrec (hashPrec p) (C# c) <> oneHash

instance GTextShowCon arity UDouble where
    gShowbPrecCon _ _ p (UDouble d) = showbPrec (hashPrec p) (D# d) <> twoHash

instance GTextShowCon arity UFloat where
    gShowbPrecCon _ _ p (UFloat f)  = showbPrec (hashPrec p) (F# f) <> oneHash

instance GTextShowCon arity UInt where
    gShowbPrecCon _ _ p (UInt i)    = showbPrec (hashPrec p) (I# i) <> oneHash

instance GTextShowCon arity UWord where
    gShowbPrecCon _ _ p (UWord w)   = showbPrec (hashPrec p) (W# w) <> twoHash

oneHash, twoHash :: Builder
hashPrec :: Int -> Int
#if __GLASGOW_HASKELL__ >= 711
oneHash  = singleton '#'
twoHash  = fromString "##"
hashPrec = const 0
#else
oneHash  = mempty
twoHash  = mempty
hashPrec = id
#endif

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

#if __GLASGOW_HASKELL__ < 702
$(Generics.deriveAll ''ConType)
#endif

#if __GLASGOW_HASKELL__ < 800
$(deriveLift ''ConType)
#endif
