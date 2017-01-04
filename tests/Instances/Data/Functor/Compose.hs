{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE PolyKinds #-}
#endif

{-|
Module:      Instances.Data.Functor.Compose
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'Arbitrary' instance for 'Compose'.
-}
module Instances.Data.Functor.Compose () where

import Data.Functor.Compose (Compose(..))
import Test.QuickCheck (Arbitrary)

deriving instance Arbitrary (f (g a)) => Arbitrary (Compose f g a)
