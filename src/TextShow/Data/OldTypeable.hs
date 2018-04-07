{-# LANGUAGE CPP               #-}

#if MIN_VERSION_base(4,7,0) && !(MIN_VERSION_base(4,8,0))
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-warnings-deprecations #-}
#endif
{-|
Module:      TextShow.Data.OldTypeable
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'TextShow' instances for data types in the @OldTypeable@ module.
This module only exports functions if using @base-4.7@.

/Since: 2/
-}
module TextShow.Data.OldTypeable () where

#if MIN_VERSION_base(4,7,0) && !(MIN_VERSION_base(4,8,0))
import Data.OldTypeable.Internal (TyCon(TyCon, tyConName), TypeRep(..),
                                  funTc, listTc)
import Data.Text.Lazy.Builder (fromString, singleton)

import Prelude ()
import Prelude.Compat

import TextShow.Classes (TextShow(..), showbParen, showbSpace)
import TextShow.Data.Typeable.Utils (showbArgs, showbTuple)
import TextShow.Utils (isTupleString)

-- | Does the 'TyCon' represent a tuple type constructor?
isTupleTyCon :: TyCon -> Bool
isTupleTyCon (TyCon _ _ _ str) = isTupleString str
{-# INLINE isTupleTyCon #-}

-- | /Since: 2/
instance TextShow TyCon where
    showb = fromString . tyConName
    {-# INLINE showb #-}

-- | /Since: 2/
instance TextShow TypeRep where
    showbPrec p (TypeRep _ tycon tys) =
        case tys of
          [] -> showb tycon
          [x]   | tycon == listTc -> singleton '[' <> showb x <> singleton ']'
          [a,r] | tycon == funTc  -> showbParen (p > 8) $
                                        showbPrec 9 a
                                     <> " -> "
                                     <> showbPrec 8 r
          xs | isTupleTyCon tycon -> showbTuple xs
             | otherwise          -> showbParen (p > 9) $
                                        showbPrec p tycon
                                     <> showbSpace
                                     <> showbArgs showbSpace tys
    {-# INLINE showbPrec #-}
#endif
