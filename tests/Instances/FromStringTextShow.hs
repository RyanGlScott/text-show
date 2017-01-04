{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module:      Instances.FromStringTextShow
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'Arbitrary' instances 'FromStringShow' and 'FromTextShow'
-}
module Instances.FromStringTextShow () where

import Test.QuickCheck (Arbitrary)
import TextShow (FromStringShow(..), FromTextShow(..),
                 FromStringShow1(..), FromStringShow2(..),
                 FromTextShow1(..), FromTextShow2(..))

deriving instance Arbitrary a       => Arbitrary (FromStringShow a)
deriving instance Arbitrary (f a)   => Arbitrary (FromStringShow1 f a)
deriving instance Arbitrary (f a b) => Arbitrary (FromStringShow2 f a b)
deriving instance Arbitrary a       => Arbitrary (FromTextShow a)
deriving instance Arbitrary (f a)   => Arbitrary (FromTextShow1 f a)
deriving instance Arbitrary (f a b) => Arbitrary (FromTextShow2 f a b)
