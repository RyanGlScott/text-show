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
module Text.Show.Text.Data.Typeable (
      showbTypeRepPrec
    , showbTyCon
#if MIN_VERSION_base(4,4,0)
    , showbFingerprint
#endif
    , showbProxy
    ) where

import Data.Monoid (mempty)
import Data.Proxy (Proxy(..))
import Data.Text.Lazy.Builder (Builder, fromString)
import Data.Typeable (TypeRep, typeRepArgs, typeRepTyCon)
#if MIN_VERSION_base(4,4,0)
import Data.Typeable.Internal (TyCon(..), funTc, listTc)
import GHC.Fingerprint.Type (Fingerprint(..))
#else
import Data.Typeable (TyCon, mkTyCon, tyConString, typeOf)
#endif
import Data.Word (Word64)

import Prelude hiding (Show)

import Text.Show.Text.Class (Show(showb, showbPrec), showbParen, showbSpace)
import Text.Show.Text.Data.Integral (showbHex)
import Text.Show.Text.Data.List ()
import Text.Show.Text.Utils ((<>), lengthB, replicateB, s)

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

-- | Helper function for showing a list of arguments, each separated by the given
-- 'Builder'.
showbArgs :: Show a => Builder -> [a] -> Builder
showbArgs _   []     = mempty
showbArgs _   [a]    = showbPrec 10 a
showbArgs sep (a:as) = showbPrec 10 a <> sep <> showbArgs sep as
{-# INLINE showbArgs #-}

-- | Helper function for showing a list of 'TypeRep's in a tuple.
showbTuple :: [TypeRep] -> Builder
showbTuple args = s '(' <> showbArgs (s ',') args <> s ')'
{-# INLINE showbTuple #-}

-- | Convert a 'TyCon' to a 'Builder'.
showbTyCon :: TyCon -> Builder
showbTyCon = fromString . tyConString
{-# INLINE showbTyCon #-}

-- | Convert a 'Proxy' type to a 'Builder'.
showbProxy :: Proxy s -> Builder
showbProxy _ = "Proxy"
{-# INLINE showbProxy #-}

#if MIN_VERSION_base(4,4,0)
-- | Convert a 'Fingerprint' to a 'Builder'.
showbFingerprint :: Fingerprint -> Builder
showbFingerprint (Fingerprint w1 w2) = hex16 w1 <> hex16 w2
  where
    hex16 :: Word64 -> Builder
    hex16 i = let hex = showbHex i
               in replicateB (16 - lengthB hex) (s '0') <> hex
{-# INLINE showbFingerprint #-}
#endif

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

#if MIN_VERSION_base(4,4,0)
instance Show Fingerprint where
    showb = showbFingerprint
    {-# INLINE showb #-}
#endif

-- TODO: See why 'deriveShow' can't detect Proxy's phantom type correctly
instance Show (Proxy s) where
    showb = showbProxy
    {-# INLINE showb #-}
