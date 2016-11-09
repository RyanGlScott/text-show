{-# LANGUAGE CPP                #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE DeriveGeneric      #-}
#endif

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module:      Instances.System.IO
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'Arbitrary' instances for data types in the "System.IO" module.
-}
module Instances.System.IO () where

#if __GLASGOW_HASKELL__ >= 702
import           GHC.Generics (Generic)
#else
import qualified Generics.Deriving.TH as Generics (deriveAll0)
#endif

#if MIN_VERSION_base(4,4,0)
import           GHC.IO.Encoding.Failure (CodingFailureMode(..))
import           GHC.IO.Encoding.Types (CodingProgress(..))
#endif

import           GHC.IO.Handle (HandlePosn(..))

import           Instances.Utils.GenericArbitrary (genericArbitrary)

import           Prelude ()
import           Prelude.Compat

import           System.IO (BufferMode(..), IOMode(..), Newline(..), NewlineMode(..),
                            SeekMode(..), Handle, stdin, stdout, stderr)

import           Test.QuickCheck (Arbitrary(..), arbitraryBoundedEnum, oneof)

instance Arbitrary Handle where
    arbitrary = oneof $ map pure [stdin, stdout, stderr]

instance Arbitrary HandlePosn where
    arbitrary = genericArbitrary

deriving instance Bounded IOMode
instance Arbitrary IOMode where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary BufferMode where
    arbitrary = genericArbitrary

deriving instance Bounded SeekMode
instance Arbitrary SeekMode where
    arbitrary = arbitraryBoundedEnum

#if MIN_VERSION_base(4,4,0)
deriving instance Bounded CodingProgress
deriving instance Enum CodingProgress
instance Arbitrary CodingProgress where
    arbitrary = arbitraryBoundedEnum

deriving instance Bounded CodingFailureMode
deriving instance Enum CodingFailureMode
instance Arbitrary CodingFailureMode where
    arbitrary = arbitraryBoundedEnum
#endif

deriving instance Bounded Newline
deriving instance Enum Newline
instance Arbitrary Newline where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary NewlineMode where
    arbitrary = genericArbitrary

#if __GLASGOW_HASKELL__ >= 702
deriving instance Generic HandlePosn
deriving instance Generic BufferMode
deriving instance Generic NewlineMode
#else
$(Generics.deriveAll0 ''HandlePosn)
$(Generics.deriveAll0 ''BufferMode)
$(Generics.deriveAll0 ''NewlineMode)
#endif
