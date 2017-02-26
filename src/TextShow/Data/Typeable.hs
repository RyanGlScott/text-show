{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE MagicHash         #-}
{-# LANGUAGE OverloadedStrings #-}

#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE PolyKinds         #-}
#endif

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module:      TextShow.Data.Typeable
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'TextShow' instances for data types in the @Typeable@ module.

/Since: 2/
-}
module TextShow.Data.Typeable () where

import Data.Monoid.Compat ((<>))
import Data.Text.Lazy.Builder (fromString, singleton)
import Data.Typeable (TypeRep, typeRepArgs, typeRepTyCon)
#if MIN_VERSION_base(4,4,0)
import Data.Typeable.Internal (tyConName)
# if MIN_VERSION_base(4,8,0)
import Data.Typeable.Internal (typeRepKinds)
# endif
# if MIN_VERSION_base(4,9,0)
import Data.Text.Lazy.Builder (Builder)
import Data.Typeable.Internal (Proxy(..), Typeable, TypeRep(TypeRep), typeRep)
import GHC.Exts (RuntimeRep(..), TYPE)
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

#if MIN_VERSION_base(4,9,0)
tyConOf :: Typeable a => Proxy a -> TyCon
tyConOf = typeRepTyCon . typeRep

tcFun :: TyCon
tcFun = tyConOf (Proxy :: Proxy (Int -> Int))

tcList :: TyCon
tcList = tyConOf (Proxy :: Proxy [])

tcTYPE :: TyCon
tcTYPE = tyConOf (Proxy :: Proxy TYPE)

tc'Lifted :: TyCon
tc'Lifted = tyConOf (Proxy :: Proxy 'PtrRepLifted)

tc'Unlifted :: TyCon
tc'Unlifted = tyConOf (Proxy :: Proxy 'PtrRepUnlifted)
#elif MIN_VERSION_base(4,4,0)
-- | The list 'TyCon'.
tcList :: TyCon
tcList = listTc

-- | The function (@->@) 'TyCon'.
tcFun :: TyCon
tcFun = funTc
#else
-- | The list 'TyCon'.
tcList :: TyCon
tcList = typeRepTyCon $ typeOf [()]

-- | The function (@->@) 'TyCon'.
tcFun :: TyCon
tcFun = mkTyCon "->"
#endif

-- | Does the 'TyCon' represent a tuple type constructor?
isTupleTyCon :: TyCon -> Bool
isTupleTyCon = isTupleString . tyConString
{-# INLINE isTupleTyCon #-}

#if MIN_VERSION_base(4,4,0)
-- | Identical to 'tyConName'. Defined to avoid using excessive amounts of pragmas
-- with base-4.3 and earlier, which use 'tyConString'.
tyConString :: TyCon -> String
tyConString = tyConName
{-# INLINE tyConString #-}
#endif

-- | /Since: 2/
instance TextShow TypeRep where
    showbPrec p tyrep =
        case tys of
          [] -> showb tycon
#if MIN_VERSION_base(4,9,0)
          [x@(TypeRep _ argCon _ _)]
#else
          [x]
#endif
            | tycon == tcList -> singleton '[' <> showb x <> singleton ']'
#if MIN_VERSION_base(4,9,0)
            | tycon == tcTYPE && argCon == tc'Lifted   -> singleton '*'
            | tycon == tcTYPE && argCon == tc'Unlifted -> singleton '#'
#endif
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

-- | /Since: 2/
instance TextShow TyCon where
#if MIN_VERSION_base(4,9,0)
    showb (TyCon _ _ _ tc_name) = showb tc_name
#else
    showb = fromString . tyConString
#endif
    {-# INLINE showb #-}

#if MIN_VERSION_base(4,9,0)
-- | Only available with @base-4.9.0.0@ or later.
--
-- /Since: 3/
instance TextShow TrName where
    showb (TrNameS s) = unpackCStringToBuilder# s
    showb (TrNameD s) = fromString s
    {-# INLINE showb #-}

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

-- | Only available with @base-4.9.0.0@ or later.
--
-- /Since: 3/
instance TextShow Module where
    showb (Module p m) = showb p <> singleton ':' <> showb m
    {-# INLINE showb #-}
#endif
