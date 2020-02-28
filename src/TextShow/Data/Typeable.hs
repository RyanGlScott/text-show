{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE MagicHash         #-}
{-# LANGUAGE OverloadedStrings #-}

#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE PolyKinds         #-}
#endif

#if __GLASGOW_HASKELL__ >= 801
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TypeApplications  #-}
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

import           Prelude ()
import           Prelude.Compat

#if MIN_VERSION_base(4,10,0)
import           Data.Kind (Type)
import           Data.Text.Lazy.Builder (Builder, fromString, singleton)
import           Data.Type.Equality ((:~~:)(..))

import           GHC.Exts (Addr#, Char(..), (+#), eqChar#, indexCharOffAddr#)
import           GHC.Types (Module(..), TrName(..), TyCon(..), isTrue#)

import           TextShow.Classes (TextShow(..), TextShow1(..), showbParen, showbSpace)
import           TextShow.Data.Typeable.Utils (showbArgs, showbTuple)
import           TextShow.Utils (isTupleString)

import           Type.Reflection (pattern App, pattern Con, pattern Con', pattern Fun,
                                  SomeTypeRep(..), TypeRep,
                                  eqTypeRep, tyConName, typeRep, typeRepTyCon)
#else /* !(MIN_VERSION_base(4,10,0) */
import           Data.Text.Lazy.Builder (fromString, singleton)
import           Data.Typeable (TypeRep, typeRepArgs, typeRepTyCon)
import           Data.Typeable.Internal (tyConName)
# if MIN_VERSION_base(4,8,0)
import           Data.Typeable.Internal (typeRepKinds)
# endif
# if MIN_VERSION_base(4,9,0)
import           Data.Text.Lazy.Builder (Builder)
import           Data.Typeable.Internal (Proxy(..), Typeable,
                                         TypeRep(TypeRep), typeRep)
import           GHC.Exts (RuntimeRep(..), TYPE)
# else
import           Data.Typeable.Internal (funTc, listTc)
# endif

# if MIN_VERSION_base(4,9,0)
import           GHC.Exts (Addr#, Char(..), (+#), eqChar#, indexCharOffAddr#)
import           GHC.Types (TyCon(..), TrName(..), Module(..), isTrue#)
# else
import           Data.Typeable.Internal (TyCon)
# endif

import           TextShow.Classes (TextShow(..), showbParen, showbSpace)
import           TextShow.Data.List ()
import           TextShow.Data.Typeable.Utils (showbArgs, showbTuple)
import           TextShow.Utils (isTupleString)
#endif

#if !(MIN_VERSION_base(4,10,0))
# if MIN_VERSION_base(4,9,0)
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
# else
-- | The list 'TyCon'.
tcList :: TyCon
tcList = listTc

-- | The function (@->@) 'TyCon'.
tcFun :: TyCon
tcFun = funTc
# endif
#endif

-- | Does the 'TyCon' represent a tuple type constructor?
isTupleTyCon :: TyCon -> Bool
isTupleTyCon = isTupleString . tyConName
{-# INLINE isTupleTyCon #-}

#if MIN_VERSION_base(4,10,0)
-- | Only available with @base-4.10.0.0@ or later.
--
-- /Since: 3.6/
instance TextShow SomeTypeRep where
    showbPrec p (SomeTypeRep ty) = showbPrec p ty

-- | Only available with @base-4.10.0.0@ or later.
--
-- /Since: 3.6/
instance TextShow (TypeRep (a :: k)) where
    showbPrec = showbTypeable

-- | Only available with @base-4.10.0.0@ or later.
--
-- /Since: 3.6/
instance TextShow1 TypeRep where
    liftShowbPrec _ _ = showbTypeable

showbTypeable :: Int -> TypeRep (a :: k) -> Builder
showbTypeable _ rep
  | Just HRefl <- rep `eqTypeRep` (typeRep :: TypeRep Type) =
    singleton '*'
  | isListTyCon tc, [ty] <- tys =
    singleton '[' <> showb ty <> singleton ']'
  | isTupleTyCon tc =
    showbTuple tys
  where (tc, tys) = splitApps rep
showbTypeable p (Con' tycon [])
  = showbPrec p tycon
showbTypeable p (Con' tycon args)
  = showbParen (p > 9) $
    showbPrec p tycon <>
    showbSpace <>
    showbArgs showbSpace args
showbTypeable p (Fun x r)
  = showbParen (p > 8) $
    showbPrec 9 x <> " -> " <> showbPrec 8 r
showbTypeable p (App f x)
  = showbParen (p > 9) $
    showbPrec 8 f <>
    showbSpace <>
    showbPrec 10 x

splitApps :: TypeRep a -> (TyCon, [SomeTypeRep])
splitApps = go []
  where
    go :: [SomeTypeRep] -> TypeRep a -> (TyCon, [SomeTypeRep])
    go xs (Con tc)  = (tc, xs)
    go xs (App f x) = go (SomeTypeRep x : xs) f
    go [] (Fun a b) = (funTyCon, [SomeTypeRep a, SomeTypeRep b])
    go _  (Fun _ _) =
        errorWithoutStackTrace "Data.Typeable.Internal.splitApps: Impossible"

funTyCon :: TyCon
funTyCon = typeRepTyCon (typeRep @(->))

isListTyCon :: TyCon -> Bool
isListTyCon tc = tc == typeRepTyCon (typeRep :: TypeRep [Int])
#else
-- | Only available with @base-4.9@ or earlier.
--
-- /Since: 2/
instance TextShow TypeRep where
    showbPrec p tyrep =
        case tys of
          [] -> showb tycon
# if MIN_VERSION_base(4,9,0)
          [x@(TypeRep _ argCon _ _)]
# else
          [x]
# endif
            | tycon == tcList -> singleton '[' <> showb x <> singleton ']'
# if MIN_VERSION_base(4,9,0)
            | tycon == tcTYPE && argCon == tc'Lifted   -> singleton '*'
            | tycon == tcTYPE && argCon == tc'Unlifted -> singleton '#'
# endif
          [a,r] | tycon == tcFun  -> showbParen (p > 8) $
                                        showbPrec 9 a
                                     <> " -> "
                                     <> showbPrec 8 r
          xs | isTupleTyCon tycon -> showbTuple xs
             | otherwise          -> showbParen (p > 9) $
                                        showbPrec p tycon
                                     <> showbSpace
                                     <> showbArgs showbSpace
# if MIN_VERSION_base(4,8,0)
                                                             (kinds ++ tys)
# else
                                                             tys
# endif
      where
        tycon = typeRepTyCon tyrep
        tys   = typeRepArgs tyrep
# if MIN_VERSION_base(4,8,0)
        kinds = typeRepKinds tyrep
# endif
#endif

-- | /Since: 2/
instance TextShow TyCon where
#if MIN_VERSION_base(4,10,0)
    showbPrec p (TyCon _ _ _ tc_name _ _) = showbPrec p tc_name
#elif MIN_VERSION_base(4,9,0)
    showb (TyCon _ _ _ tc_name) = showb tc_name
#else
    showb = fromString . tyConName
#endif

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
