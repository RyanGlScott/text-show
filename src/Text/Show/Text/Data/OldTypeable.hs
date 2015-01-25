{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-warnings-deprecations #-}
{-|
Module:      Text.Show.Text.Data.OldTypeable
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' functions for data types in the @OldTypeable@ module.
This module is only available with @base-4.7@.

/Since: 0.5/
-}
module Text.Show.Text.Data.OldTypeable (showbTyCon, showbTypeRepPrec) where

import Data.OldTypeable.Internal (TyCon(TyCon, tyConName), TypeRep(..),
                                  funTc, listTc)
import Data.Text.Lazy.Builder (Builder, fromString)

import Prelude hiding (Show)

import Text.Show.Text.Classes (Show(showb, showbPrec), showbParen, showbSpace)
import Text.Show.Text.Data.Typeable.Utils (showbArgs, showbTuple)
import Text.Show.Text.Utils ((<>), isTupleString, s)

-- | Convert a 'TyCon' to a 'Builder'.
-- This function is only available with @base-4.7@.
-- 
-- /Since: 0.5/
showbTyCon :: TyCon -> Builder
showbTyCon = fromString . tyConName
{-# INLINE showbTyCon #-}

-- | Convert a 'TypeRep' to a 'Builder' with the given precedence.
-- This function is only available with @base-4.7@.
-- 
-- /Since: 0.5/
showbTypeRepPrec :: Int -> TypeRep -> Builder
showbTypeRepPrec p (TypeRep _ tycon tys) =
    case tys of
      [] -> showbTyCon tycon
      [x]   | tycon == listTc -> s '[' <> showb x <> s ']'
      [a,r] | tycon == funTc  -> showbParen (p > 8) $
                                    showbPrec 9 a
                                 <> " -> "
                                 <> showbPrec 8 r
      xs | isTupleTyCon tycon -> showbTuple xs
         | otherwise          -> showbParen (p > 9) $
                                    showbPrec p tycon
                                 <> showbSpace
                                 <> showbArgs showbSpace tys

-- | Does the 'TyCon' represent a tuple type constructor?
isTupleTyCon :: TyCon -> Bool
isTupleTyCon (TyCon _ _ _ str) = isTupleString str
{-# INLINE isTupleTyCon #-}

instance Show TyCon where
    showb = showbTyCon
    {-# INLINE showb #-}

instance Show TypeRep where
    showbPrec = showbTypeRepPrec
    {-# INLINE showbPrec #-}
