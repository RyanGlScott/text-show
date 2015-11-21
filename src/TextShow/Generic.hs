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
    , GTextShow1(..)
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

import           TextShow.Classes (TextShow(showbPrec), TextShow1(..),
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
genericShowbPrec p = gShowbPrec Pref p . from

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
genericShowbPrecWith sp p = gShowbPrecWith Pref sp p . from1

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
-- /Since: 2/
class GTextShow f where
    -- | This is used as the default generic implementation of 'showbPrec'.
    gShowbPrec :: ConType -> Int -> f a -> Builder
    -- | Whether a representation type has any constructors.
    isNullary :: f a -> Bool
    isNullary = error "generic showbPrec (isNullary): unnecessary case"
#if __GLASGOW_HASKELL__ >= 708
    {-# MINIMAL gShowbPrec #-}

deriving instance Typeable GTextShow
#endif

instance GTextShow U1 where
    gShowbPrec _ _ U1 = mempty
    isNullary _ = True

instance TextShow c => GTextShow (K1 i c) where
    gShowbPrec _ n (K1 a) = showbPrec n a
    isNullary _ = False

instance (Constructor c, GTextShow f) => GTextShow (C1 c f) where
    gShowbPrec = gShowbConstructor gShowbPrec isNullary

instance (Selector s, GTextShow f) => GTextShow (S1 s f) where
    gShowbPrec = gShowbSelector gShowbPrec
    isNullary (M1 x) = isNullary x

instance GTextShow f => GTextShow (D1 d f) where
    gShowbPrec t n (M1 x) = gShowbPrec t n x

instance (GTextShow f, GTextShow g) => GTextShow (f :+: g) where
    gShowbPrec t n (L1 x) = gShowbPrec t n x
    gShowbPrec t n (R1 x) = gShowbPrec t n x

instance (GTextShow f, GTextShow g) => GTextShow (f :*: g) where
    gShowbPrec = gShowbProduct gShowbPrec gShowbPrec
    -- If we have a product then it is not a nullary constructor
    isNullary _ = False

instance GTextShow UChar where
    gShowbPrec _ = gShowbUCharPrec
    isNullary _ = False

instance GTextShow UDouble where
    gShowbPrec _ = gShowbUDoublePrec
    isNullary _ = False

instance GTextShow UFloat where
    gShowbPrec _ = gShowbUFloatPrec
    isNullary _ = False

instance GTextShow UInt where
    gShowbPrec _ = gShowbUIntPrec
    isNullary _ = False

instance GTextShow UWord where
    gShowbPrec _ = gShowbUWordPrec
    isNullary _ = False

-------------------------------------------------------------------------------

-- | Class of generic representation types ('Rep1') that can be converted to
-- a 'Builder' by lifting through a unary type constructor.
--
-- /Since: 2/
class GTextShow1 f where
    -- | This is used as the default generic implementation of 'showbPrecWith'.
    gShowbPrecWith :: ConType -> (Int -> a -> Builder) -> Int -> f a -> Builder
    -- | Whether a representation type has any constructors.
    isNullary1 :: f a -> Bool
    isNullary1 = error "generic showbPrecWith (isNullary1): unnecessary case"
#if __GLASGOW_HASKELL__ >= 708
    {-# MINIMAL gShowbPrecWith #-}

deriving instance Typeable GTextShow1
#endif

instance GTextShow1 U1 where
    gShowbPrecWith _ _ _ U1 = mempty
    isNullary1 _ = True

instance GTextShow1 Par1 where
    gShowbPrecWith _ sp n (Par1 p) = sp n p
    isNullary1 _ = False

instance TextShow c => GTextShow1 (K1 i c) where
    gShowbPrecWith _ _ n (K1 a) = showbPrec n a
    isNullary1 _ = False

instance TextShow1 f => GTextShow1 (Rec1 f) where
    gShowbPrecWith _ sp n (Rec1 r) = showbPrecWith sp n r
    isNullary1 _ = False

instance (Constructor c, GTextShow1 f) => GTextShow1 (C1 c f) where
    gShowbPrecWith t sp = gShowbConstructor (flip gShowbPrecWith sp) isNullary1 t

instance (Selector s, GTextShow1 f) => GTextShow1 (S1 s f) where
    gShowbPrecWith t sp = gShowbSelector (flip gShowbPrecWith sp) t
    isNullary1 (M1 x) = isNullary1 x

instance GTextShow1 f => GTextShow1 (D1 d f) where
    gShowbPrecWith t sp n (M1 x) = gShowbPrecWith t sp n x

instance (GTextShow1 f, GTextShow1 g) => GTextShow1 (f :+: g) where
    gShowbPrecWith t sp n (L1 x) = gShowbPrecWith t sp n x
    gShowbPrecWith t sp n (R1 x) = gShowbPrecWith t sp n x

instance (GTextShow1 f, GTextShow1 g) => GTextShow1 (f :*: g) where
    gShowbPrecWith t sp = gShowbProduct (flip gShowbPrecWith sp) (flip gShowbPrecWith sp) t
    -- If we have a product then it is not a nullary constructor
    isNullary1 _ = False

instance (TextShow1 f, GTextShow1 g) => GTextShow1 (f :.: g) where
    gShowbPrecWith t sp n (Comp1 c) = showbPrecWith (gShowbPrecWith t sp) n c
    isNullary1 _ = False

instance GTextShow1 UChar where
    gShowbPrecWith _ _ = gShowbUCharPrec
    isNullary1 _ = False

instance GTextShow1 UDouble where
    gShowbPrecWith _ _ = gShowbUDoublePrec
    isNullary1 _ = False

instance GTextShow1 UFloat where
    gShowbPrecWith _ _ = gShowbUFloatPrec
    isNullary1 _ = False

instance GTextShow1 UInt where
    gShowbPrecWith _ _ = gShowbUIntPrec
    isNullary1 _ = False

instance GTextShow1 UWord where
    gShowbPrecWith _ _ = gShowbUWordPrec
    isNullary1 _ = False

-------------------------------------------------------------------------------
-- Shared code between GTextShow and GTextShow1
-------------------------------------------------------------------------------

gShowbConstructor :: forall c f p. Constructor c
                  => (ConType -> Int -> f p -> Builder)
                  -> (f p -> Bool)
                  -> ConType -> Int -> C1 c f p -> Builder
gShowbConstructor gs isNull _ n c@(M1 x) = case fixity of
    Prefix -> showbParen ( n > appPrec
                           && not ( isNull x
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
        <> (if isNull x || conIsTuple c
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
