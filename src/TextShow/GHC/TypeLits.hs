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
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Monomorphic 'TextShow' functions for data types in the @GHC.TypeLits@ module.
This module only exports functions if using @base-4.6.0.0@ or later.

/Since: 2/
-}
module TextShow.GHC.TypeLits (
#if MIN_VERSION_base(4,7,0)
      showbSomeNatPrec
    , showbSomeSymbol
    ) where
#elif MIN_VERSION_base(4,6,0)
      showbIsEven
    , showbIsZero
    , showbSingPrec
    ) where
#else
    ) where
#endif

#if MIN_VERSION_base(4,6,0)

import Data.Text.Lazy.Builder (Builder)
import TextShow.Classes (TextShow(..))

# if MIN_VERSION_base(4,7,0)
import GHC.TypeLits (SomeNat(..), SomeSymbol(..), natVal, symbolVal)
import TextShow.Data.Char (showbString)
import TextShow.Data.Integral (showbIntegerPrec)
# else
import Data.Monoid.Compat ((<>))
import Data.Text.Lazy.Builder (singleton)
import GHC.TypeLits (IsEven(..), IsZero(..), Kind, Sing, SingE(fromSing))
import TextShow.Data.Integral ()
# endif

# if MIN_VERSION_base(4,7,0)
-- | Convert a 'SomeNat' value to a 'Builder' with the given precedence.
-- This function is only available with @base-4.7.0.0@ or later.
--
-- /Since: 2/
showbSomeNatPrec :: Int -> SomeNat -> Builder
showbSomeNatPrec p (SomeNat x) = showbIntegerPrec p $ natVal x
{-# INLINE showbSomeNatPrec #-}

-- | Convert a 'SomeSymbol' value to a 'Builder' with the given precedence.
-- This function is only available with @base-4.7.0.0@ or later.
--
-- /Since: 2/
showbSomeSymbol :: SomeSymbol -> Builder
showbSomeSymbol (SomeSymbol x) = showbString $ symbolVal x
{-# INLINE showbSomeSymbol #-}
# else
-- | Convert an 'IsEven' value to a 'Builder'.
-- This function is only available with @base-4.6@.
--
-- /Since: 2/
showbIsEven :: IsEven n -> Builder
showbIsEven IsEvenZero = singleton '0'
showbIsEven (IsEven x) = "(2 * " <> showb x <> singleton ')'
showbIsEven (IsOdd  x) = "(2 * " <> showb x <> " + 1)"
{-# INLINE showbIsEven #-}

-- | Convert an 'IsZero' value to a 'Builder'.
-- This function is only available with @base-4.6@.
--
-- /Since: 2/
showbIsZero :: IsZero n -> Builder
showbIsZero IsZero     = singleton '0'
showbIsZero (IsSucc n) = singleton '(' <> showb n <> " + 1)"
{-# INLINE showbIsZero #-}

-- | Convert a 'Sing' value to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbSingPrec :: (SingE (Kind :: k) rep, TextShow rep) => Int -> Sing (a :: k) -> Builder
showbSingPrec p = showbPrec p . fromSing
{-# INLINE showbSingPrec #-}
# endif

# if MIN_VERSION_base(4,7,0)
instance TextShow SomeNat where
    showbPrec = showbSomeNatPrec
    {-# INLINE showbPrec #-}

instance TextShow SomeSymbol where
    showb = showbSomeSymbol
    {-# INLINE showb #-}
# else
instance TextShow (IsEven n) where
    showb = showbIsEven
    {-# INLINE showb #-}

instance TextShow (IsZero n) where
    showb = showbIsZero
    {-# INLINE showb #-}

instance (SingE (Kind :: k) rep, TextShow rep) => TextShow (Sing (a :: k)) where
    showbPrec = showbSingPrec
    {-# INLINE showbPrec #-}
# endif

#endif
