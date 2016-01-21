{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE MagicHash         #-}
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
module TextShow.Data.Typeable (
      showbTyCon
    , showbTypeRepPrec
#if MIN_VERSION_base(4,9,0)
    , showbTrName
    , showbModule
#endif
    ) where

import Data.Monoid.Compat ((<>))
import Data.Text.Lazy.Builder (Builder, fromString, singleton)
import Data.Typeable (TypeRep, typeRepArgs, typeRepTyCon)
#if MIN_VERSION_base(4,4,0)
import Data.Typeable.Internal (tyConName)
# if MIN_VERSION_base(4,8,0)
import Data.Typeable.Internal (typeRepKinds)
# endif
# if MIN_VERSION_base(4,9,0)
import Data.Typeable.Internal (tcFun, tcList)
# elif MIN_VERSION_base(4,4,0)
import Data.Typeable.Internal (funTc, listTc)
# endif
#else
import Data.Typeable (mkTyCon, tyConString, typeOf)
#endif

#if MIN_VERSION_base(4,9,0)
import GHC.Exts (Char(..))
import GHC.Prim (Addr#, (+#), eqChar#, indexCharOffAddr#)
import GHC.Types (TyCon(..), TrName(..), Module(..), isTrue#)
#elif MIN_VERSION_base(4,4,0)
import Data.Typeable.Internal (TyCon)
#else
import Data.Typeable (TyCon)
#endif

import TextShow.Classes (TextShow(..), showbParen, showbSpace)
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
      [x]   | tycon == tcList -> singleton '[' <> showb x <> singleton ']'
      [a,r] | tycon == tcFun  -> showbParen (p > 8) $
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
tcList :: TyCon
tcList = typeRepTyCon $ typeOf [()]

-- | The function (@->@) 'TyCon'.
tcFun :: TyCon
tcFun = mkTyCon "->"
#elif !(MIN_VERSION_base(4,9,0))
-- | The list 'TyCon'.
tcList :: TyCon
tcList = listTc

-- | The function (@->@) 'TyCon'.
tcFun :: TyCon
tcFun = funTc
#endif

-- | Does the 'TyCon' represent a tuple type constructor?
isTupleTyCon :: TyCon -> Bool
isTupleTyCon = isTupleString . tyConString
{-# INLINE isTupleTyCon #-}

-- | Convert a 'TyCon' to a 'Builder'.
--
-- /Since: 2/
showbTyCon :: TyCon -> Builder
#if MIN_VERSION_base(4,9,0)
showbTyCon (TyCon _ _ _ tc_name) = showb tc_name
#else
showbTyCon = fromString . tyConString
#endif
{-# INLINE showbTyCon #-}

#if MIN_VERSION_base(4,9,0)
-- | Convert a 'TrName' to a 'Builder'.
-- This function is only available with @base-4.9.0.0@ or later.
--
-- /Since: 3/
showbTrName :: TrName -> Builder
showbTrName (TrNameS s) = unpackCStringToBuilder# s
showbTrName (TrNameD s) = fromString s

-- | Convert a 'Module' to a 'Builder'.
-- This function is only available with @base-4.9.0.0@ or later.
--
-- /Since: 3/
showbModule :: Module -> Builder
showbModule (Module p m) = showb p <> singleton ':' <> showb m
{-# INLINE showbModule #-}

unpackCStringToBuilder# :: Addr# -> Builder
    -- There's really no point in inlining this, ever, as the loop doesn't
    -- specialise in an interesting But it's pretty small, so there's a danger
    -- that it'll be inlined at every literal, which is a waste
unpackCStringToBuilder# addr
  = unpack 0#
  where
    unpack nh
      | isTrue# (ch `eqChar#` '\0'#) = mempty
      | True                         = singleton (C# ch) <> unpack (nh +# 1#)
      where
        !ch = indexCharOffAddr# addr nh
{-# NOINLINE unpackCStringToBuilder# #-}
#endif

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

#if MIN_VERSION_base(4,9,0)
instance TextShow TrName where
    showb = showbTrName
    INLINE_INST_FUN(showb)

instance TextShow Module where
    showb = showbModule
    INLINE_INST_FUN(showb)
#endif
