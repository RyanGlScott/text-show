{-# LANGUAGE GADTs           #-}
{-# LANGUAGE PolyKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-|
Module:      TextShow.Data.Type.Equality
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'TextShow' instance for propositional equality.

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

-- | /Since: 3.6/
$(deriveTextShow ''(:~~:))

-- | /Since: 3.6/
instance TextShow1 ((:~~:) a) where
    liftShowbPrec = $(makeLiftShowbPrec ''(:~~:))

-- | /Since: 3.6/
$(deriveTextShow2 ''(:~~:))
