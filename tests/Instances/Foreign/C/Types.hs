{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module:      Instances.Foreign.C.Types
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'Arbitrary' instances for data types in the "Foreign.C.Types" module.
-}
module Instances.Foreign.C.Types where

import Foreign.C.Types

import Prelude ()
import Prelude.Compat

import Test.QuickCheck (Arbitrary(..))

import TextShow (TextShow)

#if MIN_VERSION_base(4,10,0)
deriving instance Arbitrary CBool
#endif

-- The following newtypes are needed to work around a QuickCheck bug
-- (https://github.com/nick8325/quickcheck/issues/201).
-- See https://github.com/RyanGlScott/text-show/issues/36.
newtype CClockHack     = CClockHack     CClock     deriving TextShow
newtype CTimeHack      = CTimeHack      CTime      deriving TextShow
newtype CUSecondsHack  = CUSecondsHack  CUSeconds  deriving TextShow
newtype CSUSecondsHack = CSUSecondsHack CSUSeconds deriving TextShow

-- Oh DerivingStrategies, how I miss thee
instance Arbitrary CClockHack where
  arbitrary = CClockHack . CClock <$> arbitrary
instance Arbitrary CTimeHack where
  arbitrary = CTimeHack . CTime <$> arbitrary
instance Arbitrary CUSecondsHack where
  arbitrary = CUSecondsHack . CUSeconds <$> arbitrary
instance Arbitrary CSUSecondsHack where
  arbitrary = CSUSecondsHack . CSUSeconds <$> arbitrary

instance Show CClockHack where
  showsPrec p (CClockHack x) = showsPrec p x
instance Show CTimeHack where
  showsPrec p (CTimeHack x) = showsPrec p x
instance Show CUSecondsHack where
  showsPrec p (CUSecondsHack x) = showsPrec p x
instance Show CSUSecondsHack where
  showsPrec p (CSUSecondsHack x) = showsPrec p x
