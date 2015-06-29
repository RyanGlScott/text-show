{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE DeriveGeneric       #-}
#endif

#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE PolyKinds           #-}
#endif

#if __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE StandaloneDeriving  #-}
#endif
{-|
Module:      Text.Show.Text.Generic
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Generic versions of 'Show' and 'Show1' class functions, as an alternative to
"Text.Show.Text.TH", which uses Template Haskell. Because there is no 'Generic2'
class, 'T.Show2' cannot be implemented generically.

This implementation is based off of the @Generics.Deriving.Show@ module from the
@generic-deriving@ library.

/Since: 0.6/
-}
module Text.Show.Text.Generic (
      -- * Generic @show@ functions
      -- $generics

      -- ** Understanding a compiler error
      -- $generic_err
      genericShow
    , genericShowLazy
    , genericShowPrec
    , genericShowPrecLazy
    , genericShowList
    , genericShowListLazy
    , genericShowb
    , genericShowbPrec
    , genericShowbList
    , genericPrint
    , genericPrintLazy
    , genericHPrint
    , genericHPrintLazy
    , genericShowbPrecWith
    , genericShowbPrec1
      -- * The 'GShow' and 'GShow1' classes
    , GShow(..)
    , GShow1(..)
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
import           GHC.Show (appPrec, appPrec1)

import           Prelude ()
import           Prelude.Compat hiding (Show)

import           System.IO (Handle)

import qualified Text.Show as S (Show)
import qualified Text.Show.Text.Classes as T
import           Text.Show.Text.Classes (Show(showbPrec), Show1(..),
                                         showbListWith, showbParen, showbSpace)
import           Text.Show.Text.Instances ()
import           Text.Show.Text.Utils (isInfixTypeCon, isTupleString)

#include "inline.h"

{- $generics

'T.Show' instances can be easily defined for data types that are 'Generic' instances.
The easiest way to do this is to use the @DeriveGeneric@ extension.

@
&#123;-&#35; LANGUAGE DeriveGeneric &#35;-&#125;
import GHC.Generics
import Text.Show.Text
import Text.Show.Generic

data D a = D a
  deriving (Generic, Generic1)

instance Show a => Show (D a) where
    showbPrec = 'genericShowbPrec'

instance Show1 D where
    showbPrecWith = 'genericShowbPrecWith'
@
-}

{- $generic_err

Suppose you intend to use 'genericShowbPrec' to define a 'T.Show' instance.

@
data Oops1 = Oops1
    -- forgot to add \"deriving Generic\" here!

instance Show Oops1 where
    showbPrec = 'genericShowbPrec'
@

If you forget to add a @deriving 'Generic'@ clause to your data type, at
compile-time, you will get an error message that begins roughly as follows:

@
No instance for ('GShow' (Rep Oops1))
@

This error can be confusing, but don't let it intimidate you. The correct fix is
simply to add the missing \"@deriving 'Generic'@\" clause.

Similarly, if the compiler complains about not having an instance for @('GShow1'
(Rep1 Oops2))@, add a \"@deriving 'Generic1'@\" clause.
-}

-- | A 'Generic' implementation of 'T.show'.
--
-- /Since: 0.6/
genericShow :: (Generic a, GShow (Rep a)) => a -> TS.Text
genericShow = toStrict . genericShowLazy

-- | A 'Generic' implementation of 'T.showLazy'.
--
-- /Since: 0.6/
genericShowLazy :: (Generic a, GShow (Rep a)) => a -> TL.Text
genericShowLazy = toLazyText . genericShowb

-- | A 'Generic' implementation of 'T.showPrec'.
--
-- /Since: 0.6/
genericShowPrec :: (Generic a, GShow (Rep a)) => Int -> a -> TS.Text
genericShowPrec p = toStrict . genericShowPrecLazy p

-- | A 'Generic' implementation of 'T.showPrecLazy'.
--
-- /Since: 0.6/
genericShowPrecLazy :: (Generic a, GShow (Rep a)) => Int -> a -> TL.Text
genericShowPrecLazy p = toLazyText . genericShowbPrec p

-- | A 'Generic' implementation of 'T.showList'.
--
-- /Since: 0.6/
genericShowList :: (Generic a, GShow (Rep a)) => [a] -> TS.Text
genericShowList = toStrict . genericShowListLazy

-- | A 'Generic' implementation of 'T.showListLazy'.
--
-- /Since: 0.6/
genericShowListLazy :: (Generic a, GShow (Rep a)) => [a] -> TL.Text
genericShowListLazy = toLazyText . genericShowbList

-- | A 'Generic' implementation of 'T.showb'.
--
-- /Since: 0.6/
genericShowb :: (Generic a, GShow (Rep a)) => a -> Builder
genericShowb = genericShowbPrec 0

-- | A 'Generic' implementation of 'T.showbPrec'.
--
-- /Since: 0.6/
genericShowbPrec :: (Generic a, GShow (Rep a)) => Int -> a -> Builder
genericShowbPrec p = gShowbPrec Pref p . from

-- | A 'Generic' implementation of 'T.showbList'.
--
-- /Since: 0.6/
genericShowbList :: (Generic a, GShow (Rep a)) => [a] -> Builder
genericShowbList = showbListWith genericShowb

-- | A 'Generic' implementation of 'T.print'.
--
-- /Since: 0.6/
genericPrint :: (Generic a, GShow (Rep a)) => a -> IO ()
genericPrint = TS.putStrLn . genericShow

-- | A 'Generic' implementation of 'T.printLazy'.
--
-- /Since: 0.6/
genericPrintLazy :: (Generic a, GShow (Rep a)) => a -> IO ()
genericPrintLazy = TL.putStrLn . genericShowLazy

-- | A 'Generic' implementation of 'T.hPrint'.
--
-- /Since: 0.6/
genericHPrint :: (Generic a, GShow (Rep a)) => Handle -> a -> IO ()
genericHPrint h = TS.hPutStrLn h . genericShow

-- | A 'Generic' implementation of 'T.hPrintLazy'.
--
-- /Since: 0.6/
genericHPrintLazy :: (Generic a, GShow (Rep a)) => Handle -> a -> IO ()
genericHPrintLazy h = TL.hPutStrLn h . genericShowLazy

-- | A 'Generic1' implementation of 'showbPrecWith'.
--
-- /Since: 1/
genericShowbPrecWith :: (Generic1 f, GShow1 (Rep1 f))
                     => (Int -> a -> Builder) -> Int -> f a -> Builder
genericShowbPrecWith sp p = gShowbPrecWith Pref sp p . from1

-- | A 'Generic'/'Generic1' implementation of 'showbPrec1'.
--
-- /Since: 1/
genericShowbPrec1 :: (Generic a, Generic1 f, GShow (Rep a), GShow1 (Rep1 f))
                  => Int -> f a -> Builder
genericShowbPrec1 = genericShowbPrecWith genericShowbPrec

-------------------------------------------------------------------------------

-- | Whether a constructor is a record ('Rec'), a tuple ('Tup'), is prefix ('Pref'),
-- or infix ('Inf').
--
-- /Since: 0.6/
data ConType = Rec | Tup | Pref | Inf String
  deriving ( Eq
           , Ord
           , Read
           , S.Show
           , Typeable
#if __GLASGOW_HASKELL__ >= 702
           , Generic
#endif
           )

instance T.Show ConType where
    showbPrec = genericShowbPrec
    INLINE_INST_FUN(showbPrec)

-- | Class of generic representation types ('Rep') that can be converted to
-- a 'Builder'.
--
-- /Since: 0.6/
class GShow f where
    -- | This is used as the default generic implementation of 'showbPrec'.
    gShowbPrec :: ConType -> Int -> f a -> Builder
    -- | Whether a representation type has any constructors.
    isNullary :: f a -> Bool
    isNullary = error "generic showbPrec (isNullary): unnecessary case"
#if __GLASGOW_HASKELL__ >= 708
    {-# MINIMAL gShowbPrec #-}

deriving instance Typeable GShow
#endif

instance GShow U1 where
    gShowbPrec _ _ U1 = mempty
    isNullary _ = True

instance T.Show c => GShow (K1 i c) where
    gShowbPrec _ n (K1 a) = showbPrec n a
    isNullary _ = False

instance (Constructor c, GShow f) => GShow (C1 c f) where
    gShowbPrec = gShowbConstructor gShowbPrec isNullary

instance (Selector s, GShow f) => GShow (S1 s f) where
    gShowbPrec = gShowbSelector gShowbPrec
    isNullary (M1 x) = isNullary x

instance GShow f => GShow (D1 d f) where
    gShowbPrec t n (M1 x) = gShowbPrec t n x

instance (GShow f, GShow g) => GShow (f :+: g) where
    gShowbPrec t n (L1 x) = gShowbPrec t n x
    gShowbPrec t n (R1 x) = gShowbPrec t n x

instance (GShow f, GShow g) => GShow (f :*: g) where
    gShowbPrec = gShowbProduct gShowbPrec gShowbPrec
    -- If we have a product then it is not a nullary constructor
    isNullary _ = False

-------------------------------------------------------------------------------

-- | Class of generic representation types ('Rep1') that can be converted to
-- a 'Builder' by lifting through a unary type constructor.
--
-- /Since: 1/
class GShow1 f where
    -- | This is used as the default generic implementation of 'showbPrecWith'.
    gShowbPrecWith :: ConType -> (Int -> a -> Builder) -> Int -> f a -> Builder
    -- | Whether a representation type has any constructors.
    isNullary1 :: f a -> Bool
    isNullary1 = error "generic showbPrecWith (isNullary1): unnecessary case"
#if __GLASGOW_HASKELL__ >= 708
    {-# MINIMAL gShowbPrecWith #-}

deriving instance Typeable GShow1
#endif

instance GShow1 U1 where
    gShowbPrecWith _ _ _ U1 = mempty
    isNullary1 _ = True

instance GShow1 Par1 where
    gShowbPrecWith _ sp n (Par1 p) = sp n p
    isNullary1 _ = False

instance T.Show c => GShow1 (K1 i c) where
    gShowbPrecWith _ _ n (K1 a) = showbPrec n a
    isNullary1 _ = False

instance Show1 f => GShow1 (Rec1 f) where
    gShowbPrecWith _ sp n (Rec1 r) = showbPrecWith sp n r
    isNullary1 _ = False

instance (Constructor c, GShow1 f) => GShow1 (C1 c f) where
    gShowbPrecWith t sp = gShowbConstructor (flip gShowbPrecWith sp) isNullary1 t

instance (Selector s, GShow1 f) => GShow1 (S1 s f) where
    gShowbPrecWith t sp = gShowbSelector (flip gShowbPrecWith sp) t
    isNullary1 (M1 x) = isNullary1 x

instance GShow1 f => GShow1 (D1 d f) where
    gShowbPrecWith t sp n (M1 x) = gShowbPrecWith t sp n x

instance (GShow1 f, GShow1 g) => GShow1 (f :+: g) where
    gShowbPrecWith t sp n (L1 x) = gShowbPrecWith t sp n x
    gShowbPrecWith t sp n (R1 x) = gShowbPrecWith t sp n x

instance (GShow1 f, GShow1 g) => GShow1 (f :*: g) where
    gShowbPrecWith t sp = gShowbProduct (flip gShowbPrecWith sp) (flip gShowbPrecWith sp) t
    -- If we have a product then it is not a nullary constructor
    isNullary1 _ = False

instance (Show1 f, GShow1 g) => GShow1 (f :.: g) where
    gShowbPrecWith t sp n (Comp1 c) = showbPrecWith (gShowbPrecWith t sp) n c
    isNullary1 _ = False

-------------------------------------------------------------------------------
-- Shared code between GShow and GShow1
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
