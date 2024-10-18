{-# LANGUAGE CPP              #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-|
Module:      TextShow.Data.Monoid
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'TextShow' instances for 'Monoid'-related newtypes.

/Since: 2/
-}
module TextShow.Data.Monoid () where

import Data.Monoid (All, Alt, Any, Dual, First, Last, Product, Sum)

import TextShow.Classes (TextShow(..))
import TextShow.Data.Bool ()
import TextShow.Data.Maybe ()
import TextShow.TH.Internal (deriveTextShow, deriveTextShow1, makeShowbPrec)

#if MIN_VERSION_base(4,12,0)
import Data.Monoid (Ap)
#endif

-- | /Since: 2/
$(deriveTextShow  ''All)
-- | /Since: 2/
$(deriveTextShow  ''Any)
-- | /Since: 2/
$(deriveTextShow  ''Dual)
-- | /Since: 2/
$(deriveTextShow1 ''Dual)
-- | /Since: 2/
$(deriveTextShow  ''First)
-- | /Since: 2/
$(deriveTextShow1 ''First)
-- | /Since: 2/
$(deriveTextShow  ''Last)
-- | /Since: 2/
$(deriveTextShow1 ''Last)
-- | /Since: 2/
$(deriveTextShow  ''Product)
-- | /Since: 2/
$(deriveTextShow1 ''Product)
-- | /Since: 2/
$(deriveTextShow  ''Sum)
-- | /Since: 2/
$(deriveTextShow1 ''Sum)

-- | /Since: 2/
instance TextShow (f a) => TextShow (Alt f a) where
    showbPrec = $(makeShowbPrec ''Alt)

-- | /Since: 2/
$(deriveTextShow1 ''Alt)

#if MIN_VERSION_base(4,12,0)
-- | Only available with @base-4.12.0.0@ or later.
--
-- /Since: 3.7.4/
instance TextShow (f a) => TextShow (Ap f a) where
    showbPrec = $(makeShowbPrec ''Ap)

-- | Only available with @base-4.12.0.0@ or later.
--
-- /Since: 3.7.4/
$(deriveTextShow1 ''Ap)
#endif
