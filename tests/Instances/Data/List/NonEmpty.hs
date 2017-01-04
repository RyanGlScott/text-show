{-# LANGUAGE CPP             #-}

#if !(MIN_VERSION_base(4,10,0))
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
#endif

{-|
Module:      Instances.Data.List.NonEmpty
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'Show1' instance for 'NonEmpty'.
-}
module Instances.Data.List.NonEmpty () where

import Data.Orphans ()

#if !(MIN_VERSION_base(4,10,0))
import Data.List.NonEmpty (NonEmpty)
import Text.Show.Deriving (deriveShow1)

-- TODO: Replace this with a non-orphan instance
$(deriveShow1 ''NonEmpty)
#endif
