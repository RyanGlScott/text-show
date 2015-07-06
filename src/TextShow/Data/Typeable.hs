{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      TextShow.Data.Typeable
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Monomorphic 'TextShow' functions for data types in the @Typeable@ module.

/Since: 2/
-}
module TextShow.Data.Typeable (showbTyCon, showbTypeRepPrec) where

import Data.Monoid.Compat ((<>))
import Data.Text.Lazy.Builder (Builder, fromString, singleton)
import Data.Typeable (TypeRep, typeRepArgs, typeRepTyCon)
#if MIN_VERSION_base(4,4,0)
import Data.Typeable.Internal (TyCon(..), funTc, listTc)
# if MIN_VERSION_base(4,8,0)
import Data.Typeable.Internal (typeRepKinds)
# endif
#else
import Data.Typeable (TyCon, mkTyCon, tyConString, typeOf)
#endif

import TextShow.Classes (TextShow(showb, showbPrec), showbParen, showbSpace)
import TextShow.Data.List ()
import TextShow.Data.Typeable.Utils (showbArgs, showbTuple)
import TextShow.Utils (isTupleString)

#include "inline.h"

-- | Convert a 'TypeRep' to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbTypeRepPrec :: Int -> TypeRep -> Builder
showbTypeRepPrec p tyrep =
    case tys of
      [] -> showbTyCon tycon
      [x]   | tycon == listTc -> singleton '[' <> showb x <> singleton ']'
      [a,r] | tycon == funTc  -> showbParen (p > 8) $
                                    showbPrec 9 a
                                 <> " -> "
                                 <> showbPrec 8 r
      xs | isTupleTyCon tycon -> showbTuple xs
         | otherwise          -> showbParen (p > 9) $
                                    showbPrec p tycon
                                 <> showbSpace
                                 <> showbArgs showbSpace
#if MIN_VERSION_base(4,8,0)
                                                         (kinds ++ tys)
#else
                                                         tys
#endif
  where
    tycon = typeRepTyCon tyrep
    tys   = typeRepArgs tyrep
#if MIN_VERSION_base(4,8,0)
    kinds = typeRepKinds tyrep
#endif

#if !(MIN_VERSION_base(4,4,0))
-- | The list 'TyCon'.
listTc :: TyCon
listTc = typeRepTyCon $ typeOf [()]

-- | The function (@->@) 'TyCon'.
funTc :: TyCon
funTc = mkTyCon "->"
#endif

-- | Does the 'TyCon' represent a tuple type constructor?
isTupleTyCon :: TyCon -> Bool
isTupleTyCon = isTupleString . tyConString
{-# INLINE isTupleTyCon #-}

-- | Convert a 'TyCon' to a 'Builder'.
--
-- /Since: 2/
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

instance TextShow TypeRep where
    showbPrec = showbTypeRepPrec
    INLINE_INST_FUN(showbPrec)

instance TextShow TyCon where
    showb = showbTyCon
    INLINE_INST_FUN(showb)
