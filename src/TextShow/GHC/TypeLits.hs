{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-|
Module:      TextShow.GHC.TypeLits
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'TextShow' instances for data types in the @GHC.TypeLits@ module.

/Since: 2/
-}
module TextShow.GHC.TypeLits () where

import GHC.TypeLits (SomeNat(..), SomeSymbol(..), natVal, symbolVal)
#if MIN_VERSION_base(4,16,0)
import GHC.TypeLits (SomeChar(..), charVal)
#endif

import Prelude ()
import Prelude.Compat

import TextShow.Classes (TextShow(..))
import TextShow.Data.Char ()
import TextShow.Data.Integral ()

#if MIN_VERSION_base(4,18,0)
import Data.Text.Lazy.Builder (fromString)
import GHC.Show (appPrec, appPrec1)
import GHC.TypeLits ( SNat, SSymbol, SChar
                    , fromSNat, fromSSymbol, fromSChar
                    )
import TextShow.Classes (showbParen)
#endif

-- | /Since: 2/
instance TextShow SomeNat where
    showbPrec p (SomeNat x) = showbPrec p $ natVal x
    {-# INLINE showbPrec #-}

-- | /Since: 2/
instance TextShow SomeSymbol where
    showb (SomeSymbol x) = showbList $ symbolVal x
    {-# INLINE showb #-}

#if MIN_VERSION_base(4,16,0)
-- | /Since: 3.10.1/
instance TextShow SomeChar where
    showbPrec p (SomeChar x) = showbPrec p $ charVal x
    {-# INLINE showbPrec #-}
#endif

#if MIN_VERSION_base(4,18,0)
-- | /Since: 3.10.1/
instance TextShow (SNat n) where
  showbPrec p sn
    = showbParen (p > appPrec)
      ( fromString "SNat @"
     <> showbPrec appPrec1 (fromSNat sn)
      )

-- | /Since: 3.10.1/
instance TextShow (SSymbol s) where
  showbPrec p ss
    = showbParen (p > appPrec)
      ( fromString "SSymbol @"
     <> showbList (fromSSymbol ss)
      )

-- | /Since: 3.10.1/
instance TextShow (SChar c) where
  showbPrec p sc
    = showbParen (p > appPrec)
      ( fromString "SChar @"
     <> showbPrec appPrec1 (fromSChar sc)
      )
#endif
