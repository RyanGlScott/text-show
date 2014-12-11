{-# LANGUAGE CPP, NoImplicitPrelude, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      Text.Show.Text.Data.Typeable
Copyright:   (C) 2014 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: GHC

Monomorphic 'Show' functions for data types in the @Typeable@ module.
-}
module Text.Show.Text.Data.Typeable (showbTyCon, showbTypeRepPrec) where

import Data.Text.Lazy.Builder (Builder, fromString)
import Data.Typeable (TypeRep, typeRepArgs, typeRepTyCon)
#if MIN_VERSION_base(4,4,0)
import Data.Typeable.Internal (TyCon(..), funTc, listTc)
#else
import Data.Typeable (TyCon, mkTyCon, tyConString, typeOf)
#endif

import Prelude hiding (Show)

import Text.Show.Text.Classes (Show(showb, showbPrec), showbParen, showbSpace)
import Text.Show.Text.Data.List ()
import Text.Show.Text.Data.Typeable.Utils (showbArgs, showbTuple)
import Text.Show.Text.Utils ((<>), s)

-- | Convert a 'TypeRep' to a 'Builder' with the given precedence.
showbTypeRepPrec :: Int -> TypeRep -> Builder
showbTypeRepPrec p tyrep =
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
  where
    tycon = typeRepTyCon tyrep
    tys   = typeRepArgs tyrep
{-# INLINE showbTypeRepPrec #-}

#if !(MIN_VERSION_base(4,4,0))
-- | The list 'TyCon'.
listTc :: TyCon
listTc = typeRepTyCon $ typeOf [()]
{-# INLINE listTc #-}

-- | The function (@->@) 'TyCon'.
funTc :: TyCon
funTc = mkTyCon "->"
#endif

-- | Does the 'TyCon' represent a tuple type constructor?
isTupleTyCon :: TyCon -> Bool
isTupleTyCon tycon = case tyconStr of
    ('(':',':_) -> True
    _           -> False
  where
    tyconStr = tyConString tycon
{-# INLINE isTupleTyCon #-}

-- | Convert a 'TyCon' to a 'Builder'.
showbTyCon :: TyCon -> Builder
showbTyCon = fromString . tyConString
{-# INLINE showbTyCon #-}

#if MIN_VERSION_base(4,4,0)
-- | Identical to 'tyConName'. Defined to avoid using excessive amounts of pragmas
-- with base-4.3 and earlier, which use 'tyConString'.
tyConString :: TyCon -> String
tyConString = tyConName
{-# INLINE tyConString #-}
#endif

instance Show TypeRep where
    showbPrec = showbTypeRepPrec
    {-# INLINE showbPrec #-}

instance Show TyCon where
    showb = showbTyCon
    {-# INLINE showb #-}