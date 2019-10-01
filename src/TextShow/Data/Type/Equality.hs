{-# LANGUAGE CPP             #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE PolyKinds       #-}
#endif

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      TextShow.Data.Type.Equality
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'TextShow' instance for propositional equality.
Only provided if using @base-4.7.0.0@ or later.

/Since: 2/
-}
module TextShow.Data.Type.Equality () where

import Data.Type.Equality.Compat

import TextShow.Classes (TextShow1(..))
import TextShow.TH.Internal (deriveTextShow, deriveTextShow2, makeLiftShowbPrec)

-- | /Since: 2/
$(deriveTextShow ''(:~:))

-- | /Since: 2/
instance TextShow1 ((:~:) a) where
    liftShowbPrec = $(makeLiftShowbPrec ''(:~:))

-- | /Since: 2/
$(deriveTextShow2 ''(:~:))

#if MIN_VERSION_base(4,9,0)
-- | /Since: 3.6/
$(deriveTextShow ''(:~~:))

-- | /Since: 3.6/
instance TextShow1 ((:~~:) a) where
    liftShowbPrec = $(makeLiftShowbPrec ''(:~~:))

-- | /Since: 3.6/
$(deriveTextShow2 ''(:~~:))
#endif
