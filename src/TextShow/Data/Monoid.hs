{-# LANGUAGE CPP              #-}
{-# LANGUAGE TemplateHaskell  #-}
#if MIN_VERSION_base(4,8,0)
{-# LANGUAGE FlexibleContexts #-}
#endif
{-# OPTIONS_GHC -fno-warn-orphans #-}
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

import Data.Monoid.Compat (All, Any, Dual, First, Last, Product, Sum)

import TextShow.Data.Bool ()
import TextShow.Data.Maybe ()
import TextShow.TH.Internal (deriveTextShow, deriveTextShow1)

#if MIN_VERSION_base(4,8,0)
import Data.Monoid (Alt)
import TextShow.Classes (TextShow(..))
import TextShow.TH.Internal (makeShowbPrec)
#endif

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

#if MIN_VERSION_base(4,8,0)
-- | Only available with @base-4.8.0.0@ or later.
--
-- /Since: 2/
instance TextShow (f a) => TextShow (Alt f a) where
    showbPrec = $(makeShowbPrec ''Alt)

-- | Only available with @base-4.8.0.0@ or later.
--
-- /Since: 2/
$(deriveTextShow1 ''Alt)
#endif

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
