{-# LANGUAGE CPP                  #-}

#if MIN_VERSION_base(4,6,0)
# if !(MIN_VERSION_base(4,7,0))
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE UndecidableInstances #-}
# endif

{-# OPTIONS_GHC -fno-warn-orphans #-}
#endif
{-|
Module:      TextShow.GHC.TypeLits
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'TextShow' instances for data types in the @GHC.TypeLits@ module.
Only provided if using @base-4.6.0.0@ or later.

/Since: 2/
-}
module TextShow.GHC.TypeLits () where

#if MIN_VERSION_base(4,6,0)
import Prelude ()
import Prelude.Compat

import TextShow.Classes (TextShow(..))
import TextShow.Data.Integral ()

# if MIN_VERSION_base(4,7,0)
import GHC.TypeLits (SomeNat(..), SomeSymbol(..), natVal, symbolVal)
import TextShow.Data.Char ()
# else
import Data.Text.Lazy.Builder (singleton)
import GHC.TypeLits (IsEven(..), IsZero(..), Kind, Sing, SingE(fromSing))
# endif

# if MIN_VERSION_base(4,7,0)
-- | Only available with @base-4.7.0.0@ or later.
--
-- /Since: 2/
instance TextShow SomeNat where
    showbPrec p (SomeNat x) = showbPrec p $ natVal x
    {-# INLINE showbPrec #-}

-- | Only available with @base-4.7.0.0@ or later.
--
-- /Since: 2/
instance TextShow SomeSymbol where
    showb (SomeSymbol x) = showbList $ symbolVal x
    {-# INLINE showb #-}
# else
-- | Only available with @base-4.6@.
--
-- /Since: 2/
instance TextShow (IsEven n) where
    showb IsEvenZero = singleton '0'
    showb (IsEven x) = "(2 * " <> showb x <> singleton ')'
    showb (IsOdd  x) = "(2 * " <> showb x <> " + 1)"
    {-# INLINE showb #-}

-- | Only available with @base-4.6@.
--
-- /Since: 2/
instance TextShow (IsZero n) where
    showb IsZero     = singleton '0'
    showb (IsSucc n) = singleton '(' <> showb n <> " + 1)"
    {-# INLINE showb #-}

-- | Only available with @base-4.6@.
--
-- /Since: 2/
instance (SingE (Kind :: k) rep, TextShow rep) => TextShow (Sing (a :: k)) where
    showbPrec p = showbPrec p . fromSing
    {-# INLINE showbPrec #-}
# endif

#endif
