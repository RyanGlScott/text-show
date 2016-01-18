{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MagicHash            #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}

#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE DeriveGeneric        #-}
#endif

#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE PolyKinds            #-}
#endif

{-|
Module:      TextShow.Generic
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Generic versions of 'TextShow' and 'TextShow1' class functions, as an alternative to
"TextShow.TH", which uses Template Haskell. Because there is no 'Generic2'
class, 'TextShow2' cannot be implemented generically.

This implementation is based off of the @Generics.Deriving.Show@ module from the
@generic-deriving@ library.

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
    , genericShowbPrecWith
    , genericShowbPrec1
      -- * The 'GTextShow' and 'GTextShow1' classes
    , GTextShow(..)
    , GTextShowCon(..)
    , GTextShow1(..)
    , GTextShow1Con(..)
    , IsNullary(..)
    , ConType(..)
    ) where

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
    showbPrecWith = 'genericShowbPrecWith'
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
compile-time, you will get an error message that begins roughly as follows:

@
No instance for ('GTextShow' (Rep Oops))
@

This error can be confusing, but don't let it intimidate you. The correct fix is
simply to add the missing \"@deriving 'Generic'@\" clause.

Similarly, if the compiler complains about not having an instance for @('GTextShow1'
(Rep1 Oops1))@, add a \"@deriving 'Generic1'@\" clause.
-}

-- | A 'Generic' implementation of 'showt'.
--
-- /Since: 2/
genericShowt :: (Generic a, GTextShow (Rep a)) => a -> TS.Text
genericShowt = toStrict . genericShowtl

-- | A 'Generic' implementation of 'showtl'.
--
-- /Since: 2/
genericShowtl :: (Generic a, GTextShow (Rep a)) => a -> TL.Text
genericShowtl = toLazyText . genericShowb

-- | A 'Generic' implementation of 'showPrect'.
--
-- /Since: 2/
genericShowtPrec :: (Generic a, GTextShow (Rep a)) => Int -> a -> TS.Text
genericShowtPrec p = toStrict . genericShowtlPrec p

-- | A 'Generic' implementation of 'showtlPrec'.
--
-- /Since: 2/
genericShowtlPrec :: (Generic a, GTextShow (Rep a)) => Int -> a -> TL.Text
genericShowtlPrec p = toLazyText . genericShowbPrec p

-- | A 'Generic' implementation of 'showtList'.
--
-- /Since: 2/
genericShowtList :: (Generic a, GTextShow (Rep a)) => [a] -> TS.Text
genericShowtList = toStrict . genericShowtlList

-- | A 'Generic' implementation of 'showtlList'.
--
-- /Since: 2/
genericShowtlList :: (Generic a, GTextShow (Rep a)) => [a] -> TL.Text
genericShowtlList = toLazyText . genericShowbList

-- | A 'Generic' implementation of 'showb'.
--
-- /Since: 2/
genericShowb :: (Generic a, GTextShow (Rep a)) => a -> Builder
genericShowb = genericShowbPrec 0

-- | A 'Generic' implementation of 'showbPrec'.
--
-- /Since: 2/
genericShowbPrec :: (Generic a, GTextShow (Rep a)) => Int -> a -> Builder
genericShowbPrec p = gShowbPrec p . from

-- | A 'Generic' implementation of 'showbList'.
--
-- /Since: 2/
genericShowbList :: (Generic a, GTextShow (Rep a)) => [a] -> Builder
genericShowbList = showbListWith genericShowb

-- | A 'Generic' implementation of 'printT'.
--
-- /Since: 2/
genericPrintT :: (Generic a, GTextShow (Rep a)) => a -> IO ()
genericPrintT = TS.putStrLn . genericShowt

-- | A 'Generic' implementation of 'printTL'.
--
-- /Since: 2/
genericPrintTL :: (Generic a, GTextShow (Rep a)) => a -> IO ()
genericPrintTL = TL.putStrLn . genericShowtl

-- | A 'Generic' implementation of 'hPrintT'.
--
-- /Since: 2/
genericHPrintT :: (Generic a, GTextShow (Rep a)) => Handle -> a -> IO ()
genericHPrintT h = TS.hPutStrLn h . genericShowt

-- | A 'Generic' implementation of 'hPrintTL'.
--
-- /Since: 2/
genericHPrintTL :: (Generic a, GTextShow (Rep a)) => Handle -> a -> IO ()
genericHPrintTL h = TL.hPutStrLn h . genericShowtl

-- | A 'Generic1' implementation of 'showbPrecWith'.
--
-- /Since: 2/
genericShowbPrecWith :: (Generic1 f, GTextShow1 (Rep1 f))
                     => (Int -> a -> Builder) -> Int -> f a -> Builder
genericShowbPrecWith sp p = gShowbPrecWith sp p . from1

-- | A 'Generic'/'Generic1' implementation of 'showbPrec1'.
--
-- /Since: 2/
genericShowbPrec1 :: (Generic a, Generic1 f, GTextShow (Rep a), GTextShow1 (Rep1 f))
                  => Int -> f a -> Builder
genericShowbPrec1 = genericShowbPrecWith genericShowbPrec

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
           )

instance TextShow ConType where
    showbPrec = genericShowbPrec
    INLINE_INST_FUN(showbPrec)

-- | Class of generic representation types ('Rep') that can be converted to
-- a 'Builder'.
--
-- /Since: 3/
class GTextShow f where
    -- | This is used as the default generic implementation of 'showbPrec'.
    gShowbPrec :: Int -> f a -> Builder

#if __GLASGOW_HASKELL__ >= 708
deriving instance Typeable GTextShow
#endif

instance GTextShow f => GTextShow (D1 d f) where
    gShowbPrec n (M1 x) = gShowbPrec n x

instance (GTextShow f, GTextShow g) => GTextShow (f :+: g) where
    gShowbPrec n (L1 x) = gShowbPrec n x
    gShowbPrec n (R1 x) = gShowbPrec n x

instance (Constructor c, GTextShowCon f, IsNullary f) => GTextShow (C1 c f) where
    gShowbPrec = gShowbConstructor gShowbPrecCon

-- | Class of generic representation types ('Rep') for which the 'ConType'
-- has been determined.
class GTextShowCon f where
    -- | Convert value of a specific 'ConType' to a 'Builder' with the given
    -- precedence.
    gShowbPrecCon :: ConType -> Int -> f a -> Builder

#if __GLASGOW_HASKELL__ >= 708
deriving instance Typeable GTextShowCon
#endif

instance GTextShowCon V1 where
    gShowbPrecCon = error "Void showbPrec"

instance GTextShowCon U1 where
    gShowbPrecCon _ _ U1 = mempty

instance TextShow c => GTextShowCon (K1 i c) where
    gShowbPrecCon _ n (K1 a) = showbPrec n a

instance (Selector s, GTextShowCon f) => GTextShowCon (S1 s f) where
    gShowbPrecCon = gShowbSelector gShowbPrecCon

instance (GTextShowCon f, GTextShowCon g) => GTextShowCon (f :*: g) where
    gShowbPrecCon = gShowbProduct gShowbPrecCon gShowbPrecCon

instance GTextShowCon UChar where
    gShowbPrecCon _ = gShowbUCharPrec

instance GTextShowCon UDouble where
    gShowbPrecCon _ = gShowbUDoublePrec

instance GTextShowCon UFloat where
    gShowbPrecCon _ = gShowbUFloatPrec

instance GTextShowCon UInt where
    gShowbPrecCon _ = gShowbUIntPrec

instance GTextShowCon UWord where
    gShowbPrecCon _ = gShowbUWordPrec

-------------------------------------------------------------------------------

-- | Class of generic representation types ('Rep1') that can be converted to
-- a 'Builder' by lifting through a unary type constructor.
--
-- /Since: 2/
class GTextShow1 f where
    -- | This is used as the default generic implementation of 'showbPrecWith'.
    gShowbPrecWith :: (Int -> a -> Builder) -> Int -> f a -> Builder

#if __GLASGOW_HASKELL__ >= 708
deriving instance Typeable GTextShow1
#endif

instance GTextShow1 f => GTextShow1 (D1 d f) where
    gShowbPrecWith sp n (M1 x) = gShowbPrecWith sp n x

instance (GTextShow1 f, GTextShow1 g) => GTextShow1 (f :+: g) where
    gShowbPrecWith sp n (L1 x) = gShowbPrecWith sp n x
    gShowbPrecWith sp n (R1 x) = gShowbPrecWith sp n x

instance (Constructor c, GTextShow1Con f, IsNullary f) => GTextShow1 (C1 c f) where
    gShowbPrecWith sp = gShowbConstructor (`gShowbPrecWithCon` sp)

-- | Class of generic representation types ('Rep1') for which the 'ConType'
-- has been determined.
class GTextShow1Con f where
    -- | Convert a value of a specific 'ConType' to a 'Builder' with the given
    -- show function and precedence.
    gShowbPrecWithCon :: ConType -> (Int -> a -> Builder) -> Int -> f a -> Builder

#if __GLASGOW_HASKELL__ >= 708
deriving instance Typeable GTextShow1Con
#endif

instance GTextShow1Con V1 where
    gShowbPrecWithCon = error "Void showbPrecWith"

instance GTextShow1Con U1 where
    gShowbPrecWithCon _ _ _ U1 = mempty

instance GTextShow1Con Par1 where
    gShowbPrecWithCon _ sp n (Par1 p) = sp n p

instance TextShow c => GTextShow1Con (K1 i c) where
    gShowbPrecWithCon _ _ n (K1 a) = showbPrec n a

instance TextShow1 f => GTextShow1Con (Rec1 f) where
    gShowbPrecWithCon _ sp n (Rec1 r) = showbPrecWith sp n r

instance (Selector s, GTextShow1Con f) => GTextShow1Con (S1 s f) where
    gShowbPrecWithCon t sp = gShowbSelector (`gShowbPrecWithCon` sp) t

instance (GTextShow1Con f, GTextShow1Con g) => GTextShow1Con (f :*: g) where
    gShowbPrecWithCon t sp = gShowbProduct (`gShowbPrecWithCon` sp) (`gShowbPrecWithCon` sp) t

instance (TextShow1 f, GTextShow1Con g) => GTextShow1Con (f :.: g) where
    gShowbPrecWithCon t sp n (Comp1 c) = showbPrecWith (gShowbPrecWithCon t sp) n c

instance GTextShow1Con UChar where
    gShowbPrecWithCon _ _ = gShowbUCharPrec

instance GTextShow1Con UDouble where
    gShowbPrecWithCon _ _ = gShowbUDoublePrec

instance GTextShow1Con UFloat where
    gShowbPrecWithCon _ _ = gShowbUFloatPrec

instance GTextShow1Con UInt where
    gShowbPrecWithCon _ _ = gShowbUIntPrec

instance GTextShow1Con UWord where
    gShowbPrecWithCon _ _ = gShowbUWordPrec

-------------------------------------------------------------------------------
-- Shared code between GTextShow and GTextShow1
-------------------------------------------------------------------------------

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

gShowbConstructor :: forall c f p.
                     (Constructor c, IsNullary f)
                  => (ConType -> Int -> f p -> Builder)
                  -> Int -> C1 c f p -> Builder
gShowbConstructor gs n c@(M1 x) = case fixity of
    Prefix -> showbParen ( n > appPrec
                           && not ( isNullary x
                                    || conIsTuple c
#if __GLASGOW_HASKELL__ >= 711
                                    || conIsRecord c
#endif
                                  )
                         ) $
           (if conIsTuple c
               then mempty
               else let cn = conName c
                    in showbParen (isInfixTypeCon cn) $ fromString cn
           )
        <> (if isNullary x || conIsTuple c
               then mempty
               else singleton ' '
           )
        <> showbBraces t (gs t appPrec1 x)
    Infix _ m -> showbParen (n > m) . showbBraces t $ gs t (m+1) x
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

gShowbSelector :: Selector s
               => (ConType -> Int -> f p -> Builder)
               -> ConType -> Int -> S1 s f p -> Builder
gShowbSelector gs t n sel@(M1 x)
    | selName sel == "" = gs t n x
    | otherwise         = fromString (selName sel) <> " = " <> gs t 0 x

gShowbProduct :: (ConType -> Int -> f p -> Builder)
              -> (ConType -> Int -> g p -> Builder)
              -> ConType -> Int -> ((f :*: g) p) -> Builder
gShowbProduct gsa gsb t@Rec _ (a :*: b) =
       gsa t 0 a
    <> ", "
    <> gsb t 0 b
gShowbProduct gsa gsb t@(Inf o) n (a :*: b) =
       gsa t n a
    <> showbSpace
    <> infixOp
    <> showbSpace
    <> gsb t n b
  where
    infixOp :: Builder
    infixOp = if isInfixTypeCon o
                 then fromString o
                 else singleton '`' <> fromString o <> singleton '`'
gShowbProduct gsa gsb t@Tup _ (a :*: b) =
       gsa t 0 a
    <> singleton ','
    <> gsb t 0 b
gShowbProduct gsa gsb t@Pref n (a :*: b) =
       gsa t n a
    <> showbSpace
    <> gsb t n b

gShowbUCharPrec :: Int -> UChar p -> Builder
gShowbUCharPrec p (UChar c) = showbPrec (hashPrec p) (C# c) <> oneHash

gShowbUDoublePrec :: Int -> UDouble p -> Builder
gShowbUDoublePrec p (UDouble d) = showbPrec (hashPrec p) (D# d) <> twoHash

gShowbUFloatPrec :: Int -> UFloat p -> Builder
gShowbUFloatPrec p (UFloat f) = showbPrec (hashPrec p) (F# f) <> oneHash

gShowbUIntPrec :: Int -> UInt p -> Builder
gShowbUIntPrec p (UInt i) = showbPrec (hashPrec p) (I# i) <> oneHash

gShowbUWordPrec :: Int -> UWord p -> Builder
gShowbUWordPrec p (UWord w) = showbPrec (hashPrec p) (W# w) <> twoHash

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

-------------------------------------------------------------------------------

#if __GLASGOW_HASKELL__ < 702
$(Generics.deriveAll ''ConType)
#endif
