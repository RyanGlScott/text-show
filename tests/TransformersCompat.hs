{-# LANGUAGE CPP                        #-}
-- {-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}

#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE DeriveGeneric              #-}
#endif

#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE PolyKinds                  #-}
#endif

{-|
Module:      TransformersCompat
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Defines the 'Show1' and 'Show2' classes for @String@s. This module will be removed
once the next version of @transformers@/@transformers-compat@ is released.
-}
module TransformersCompat (
    -- * Liftings of Prelude classes
    -- ** For unary constructors
    Show1(..), showsPrec1,
    -- ** For binary constructors
    Show2(..), showsPrec2,
    -- * Helper functions
    showsUnaryWith,
    showsBinaryWith,
    -- * Conversion between @String@ and @Text@ 'Show1'/'Show2'
    FromStringShow1(..),
    FromStringShow2(..)
    ) where

#include "generic.h"
#include "inline.h"

import            Control.Applicative (Const(..))

-- #if __GLASGOW_HASKELL__ >= 708
-- import            Data.Data (Data, Typeable)
-- #endif
import            Data.Functor.Identity (Identity(..))
import            Data.Text.Lazy.Builder (Builder)

#if __GLASGOW_HASKELL__ >= 702
import            GHC.Generics (Generic)
# if defined(__LANGUAGE_DERIVE_GENERIC1__)
import            GHC.Generics (Generic1)
# endif
#endif

import            Prelude ()
import            Prelude.Compat

import           Text.Read (Read(..), readListPrecDefault)
import qualified Text.Show.Text as T (Show, Show1, Show2)
import           Text.Show.Text (fromString, showbPrec, showbPrecWith,
                                 showbPrecWith2, toString)

-- | Lifting of the 'Show' class to unary type constructors.
class Show1 f where
    -- | Lift a 'showsPrec' function through the type constructor.
    showsPrecWith :: (Int -> a -> ShowS) -> Int -> f a -> ShowS

-- | Lift the standard 'showsPrec' function through the type constructor.
showsPrec1 :: (Show1 f, Show a) => Int -> f a -> ShowS
showsPrec1 = showsPrecWith showsPrec

newtype FromStringShow1 f a = FromStringShow1 { fromStringShow1 :: f a }
  deriving ( Eq
#if __GLASGOW_HASKELL__ >= 702
           , Generic
# if defined(__LANGUAGE_DERIVE_GENERIC1__)
           , Generic1
# endif
#endif
           , Ord
           , Show1
           )

deriving instance Functor     f => Functor     (FromStringShow1 f)
deriving instance Foldable    f => Foldable    (FromStringShow1 f)
deriving instance Traversable f => Traversable (FromStringShow1 f)
-- #if __GLASGOW_HASKELL__ >= 708
-- deriving instance Typeable FromStringShow1
-- deriving instance (Data (f a), Typeable f, Typeable a) =>
--     Data (FromStringShow1 f a)
-- #endif

-- | Convert a @Builder@-based show function to a @ShowS@-based one.
showbToShows :: (Int -> a -> Builder) -> Int -> a -> ShowS
showbToShows sp p = showString . toString . sp p

-- | Convert a @ShowS@-based show function to a @Builder@-based one.
showsToShowb :: (Int -> a -> ShowS) -> Int -> a -> Builder
showsToShowb sp p x = fromString $ sp p x ""

instance Read (f a) => Read (FromStringShow1 f a) where
    readPrec = FromStringShow1 <$> readPrec
    INLINE_INST_FUN(readPrec)

    readListPrec = readListPrecDefault
    INLINE_INST_FUN(readListPrec)

instance (Show1 f, Show a) => T.Show (FromStringShow1 f a) where
    showbPrec = showbPrecWith (showsToShowb showsPrec)
    INLINE_INST_FUN(showbPrec)

instance Show1 f => T.Show1 (FromStringShow1 f) where
    showbPrecWith sp p (FromStringShow1 x) =
        fromString $ showsPrecWith (showbToShows sp) p x ""
    INLINE_INST_FUN(showbPrecWith)

instance (Show1 f, Show a) => Show (FromStringShow1 f a) where
    showsPrec = showsPrec1
    INLINE_INST_FUN(showsPrec)

-- | Lifting of the 'Show' class to binary type constructors.
class Show2 f where
    -- | Lift 'showsPrec' functions through the type constructor.
    showsPrecWith2 :: (Int -> a -> ShowS) -> (Int -> b -> ShowS) ->
        Int -> f a b -> ShowS

-- | Lift the standard 'showsPrec' function through the type constructor.
showsPrec2 :: (Show2 f, Show a, Show b) => Int -> f a b -> ShowS
showsPrec2 = showsPrecWith2 showsPrec showsPrec

newtype FromStringShow2 f a b = FromStringShow2 (f a b)
  deriving ( Eq
#if __GLASGOW_HASKELL__ >= 702
           , Generic
# if defined(__LANGUAGE_DERIVE_GENERIC1__)
           , Generic1
# endif
#endif
           , Ord
           , Show2
           )

deriving instance Functor     (f a) => Functor     (FromStringShow2 f a)
deriving instance Foldable    (f a) => Foldable    (FromStringShow2 f a)
deriving instance Traversable (f a) => Traversable (FromStringShow2 f a)
-- #if __GLASGOW_HASKELL__ >= 708
-- deriving instance Typeable FromStringShow2
-- deriving instance (Data (f a b), Typeable f, Typeable a, Typeable b) =>
--     Data (FromStringShow2 f a b)
-- #endif

instance Read (f a b) => Read (FromStringShow2 f a b) where
    readPrec = FromStringShow2 <$> readPrec
    INLINE_INST_FUN(readPrec)

    readListPrec = readListPrecDefault
    INLINE_INST_FUN(readListPrec)

instance (Show2 f, Show a, Show b) => T.Show (FromStringShow2 f a b) where
    showbPrec = showbPrecWith (showsToShowb showsPrec)
    INLINE_INST_FUN(showbPrec)

instance (Show2 f, Show a) => T.Show1 (FromStringShow2 f a) where
    showbPrecWith = showbPrecWith2 (showsToShowb showsPrec)
    INLINE_INST_FUN(showbPrecWith)

instance Show2 f => T.Show2 (FromStringShow2 f) where
    showbPrecWith2 sp1 sp2 p (FromStringShow2 x) =
        fromString $ showsPrecWith2 (showbToShows sp1) (showbToShows sp2) p x ""
    INLINE_INST_FUN(showbPrecWith2)

instance (Show2 f, Show a, Show b) => Show (FromStringShow2 f a b) where
    showsPrec = showsPrec2
    INLINE_INST_FUN(showsPrec)

instance (Show2 f, Show a) => Show1 (FromStringShow2 f a) where
    showsPrecWith = showsPrecWith2 showsPrec
    INLINE_INST_FUN(showsPrecWith)

-------------------------------------------------------------------------------

-- | @'showsUnaryWith' sp n d x@ produces the string representation of a
-- unary data constructor with name @n@ and argument @x@, in precedence
-- context @d@.
showsUnaryWith :: (Int -> a -> ShowS) -> String -> Int -> a -> ShowS
showsUnaryWith sp name d x = showParen (d > 10) $
    showString name . showChar ' ' . sp 11 x

-- | @'showsBinaryWith' sp1 sp2 n d x y@ produces the string
-- representation of a binary data constructor with name @n@ and arguments
-- @x@ and @y@, in precedence context @d@.
showsBinaryWith :: (Int -> a -> ShowS) -> (Int -> b -> ShowS) ->
    String -> Int -> a -> b -> ShowS
showsBinaryWith sp1 sp2 name d x y = showParen (d > 10) $
    showString name . showChar ' ' . sp1 11 x . showChar ' ' . sp2 11 y

-------------------------------------------------------------------------------

instance Show a => Show1 ((,) a) where
    showsPrecWith = showsPrecWith2 showsPrec

instance Show a => Show1 (Either a) where
    showsPrecWith = showsPrecWith2 showsPrec

instance Show a => Show1 (Const a) where
    showsPrecWith = showsPrecWith2 showsPrec

instance Show1 Maybe where
    showsPrecWith _  _ Nothing  = showString "Nothing"
    showsPrecWith sp d (Just x) = showsUnaryWith sp "Just" d x

instance Show1 [] where
    showsPrecWith _  _ []     = showString "[]"
    showsPrecWith sp _ (x:xs) = showChar '[' . sp 0 x . showl xs
      where
        showl []     = showChar ']'
        showl (y:ys) = showChar ',' . sp 0 y . showl ys

instance Show1 Identity where
    showsPrecWith sp d (Identity x) = showsUnaryWith sp "Identity" d x

instance Show2 (,) where
    showsPrecWith2 sp1 sp2 _ (x, y) =
        showChar '(' . sp1 0 x . showChar ',' . sp2 0 y . showChar ')'

instance Show2 Either where
    showsPrecWith2 sp1 _   d (Left x)  = showsUnaryWith sp1 "Left" d x
    showsPrecWith2 _   sp2 d (Right x) = showsUnaryWith sp2 "Right" d x

instance Show2 Const where
    showsPrecWith2 sp _ d (Const x) = showsUnaryWith sp "Const" d x
