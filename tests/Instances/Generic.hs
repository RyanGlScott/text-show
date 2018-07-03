{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}
#if __GLASGOW_HASKELL__ >= 806
{-# LANGUAGE DerivingVia        #-}
#endif
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module:      Instances.Generic
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Provides instances for 'GenericExample', and an 'Arbitrary' instance for 'ConType'.
-}
module Instances.Generic () where

import GHC.Generics (Generic)
#if __GLASGOW_HASKELL__ >= 706
import GHC.Generics (Generic1)
#else
import Generics.Deriving.TH (deriveAll1)
#endif

import Instances.Data.Text ()
import Instances.Utils (GenericExample(..))
import Instances.Utils.GenericArbitrary (genericArbitrary)

import Test.QuickCheck (Arbitrary(..))

import Text.Show.Deriving (deriveShow1)

import TextShow (TextShow(..), TextShow1(..))
import TextShow.Generic ( ConType(..)
#if __GLASGOW_HASKELL__ >= 806
                        , FromGeneric(..), FromGeneric1(..)
#else
                        , genericShowbPrec, genericLiftShowbPrec
#endif
                        )

deriving instance Show a => Show (GenericExample a)
$(deriveShow1 ''GenericExample)
instance Arbitrary a => Arbitrary (GenericExample a) where
  arbitrary = genericArbitrary

deriving instance Generic (GenericExample a)
#if __GLASGOW_HASKELL__ >= 706
deriving instance Generic1 GenericExample
#else
$(deriveAll1 ''GenericExample)
#endif

#if __GLASGOW_HASKELL__ >= 806
deriving via FromGeneric (GenericExample a)
  instance TextShow a => TextShow (GenericExample a)
deriving via FromGeneric1 GenericExample
  instance TextShow1 GenericExample
#else
instance TextShow a => TextShow (GenericExample a) where
  showbPrec = genericShowbPrec

instance TextShow1 GenericExample where
  liftShowbPrec = genericLiftShowbPrec
#endif

instance Arbitrary ConType where
    arbitrary = genericArbitrary
