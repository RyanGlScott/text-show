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
    showsBinaryWith
    ) where

import Control.Applicative (Const(..))
import Data.Functor.Identity (Identity(..))

-- | Lifting of the 'Show' class to unary type constructors.
class Show1 f where
    -- | Lift a 'showsPrec' function through the type constructor.
    showsPrecWith :: (Int -> a -> ShowS) -> Int -> f a -> ShowS

-- | Lift the standard 'showsPrec' function through the type constructor.
showsPrec1 :: (Show1 f, Show a) => Int -> f a -> ShowS
showsPrec1 = showsPrecWith showsPrec

-- | Lifting of the 'Show' class to binary type constructors.
class Show2 f where
    -- | Lift 'showsPrec' functions through the type constructor.
    showsPrecWith2 :: (Int -> a -> ShowS) -> (Int -> b -> ShowS) ->
        Int -> f a b -> ShowS

-- | Lift the standard 'showsPrec' function through the type constructor.
showsPrec2 :: (Show2 f, Show a, Show b) => Int -> f a b -> ShowS
showsPrec2 = showsPrecWith2 showsPrec showsPrec

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
