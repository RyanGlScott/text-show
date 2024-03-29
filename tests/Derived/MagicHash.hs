{-# LANGUAGE CPP             #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

{-|
Module:      Derived.MagicHash
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Defines data types with fields that have unlifted types.
-}
module Derived.MagicHash (
    TyCon#(..), TyFamily#(..)
#if MIN_VERSION_base(4,13,0)
  , TyCon'#(..), TyFamily'#(..)
#endif
  ) where

#if __GLASGOW_HASKELL__ < 711
import qualified Generics.Deriving.TH as Generics
#endif

import           GHC.Exts
#if __GLASGOW_HASKELL__ >= 711
import           GHC.Generics (Generic, Generic1)
#endif

import           Instances.Utils.GenericArbitrary (genericArbitrary)

import           Prelude ()
import           Prelude.Compat

import           Test.QuickCheck (Arbitrary(..))

import           Text.Show.Deriving (deriveShow1Options, legacyShowOptions)
#if defined(NEW_FUNCTOR_CLASSES)
import           Text.Show.Deriving (deriveShow2Options)
#endif

import           TextShow.TH (deriveTextShow, deriveTextShow1, deriveTextShow2)
-------------------------------------------------------------------------------

data TyCon# a b = TyCon# {
    tcA       :: a
  , tcB       :: b
  , tcInt#    :: Int#
  , tcFloat#  :: Float#
  , tcDouble# :: Double#
  , tcChar#   :: Char#
  , tcWord#   :: Word#
} deriving ( Show
#if __GLASGOW_HASKELL__ >= 711
           , Generic
           , Generic1
#endif
           )

#if MIN_VERSION_base(4,13,0)
data TyCon'# a b = TyCon'# {
    tcA'      :: a
  , tcB'      :: b
  , tcInt8#   :: Int8#
  , tcInt16#  :: Int16#
  , tcWord8#  :: Word8#
  , tcWord16# :: Word16#
# if MIN_VERSION_base(4,16,0)
  , tcInt32#  :: Int32#
  , tcWord32# :: Word32#
# endif
} deriving Show
#endif

-------------------------------------------------------------------------------

data family TyFamily# y z :: *

data instance TyFamily# a b = TyFamily# {
    tfA       :: a
  , tfB       :: b
  , tfInt#    :: Int#
  , tfFloat#  :: Float#
  , tfDouble# :: Double#
  , tfChar#   :: Char#
  , tfWord#   :: Word#
} deriving ( Show
#if __GLASGOW_HASKELL__ >= 711
           , Generic
           , Generic1
#endif
           )

#if MIN_VERSION_base(4,13,0)
data family TyFamily'# y z :: *

data instance TyFamily'# a b = TyFamily'# {
    tfA'      :: a
  , tfB'      :: b
  , tfInt8#   :: Int8#
  , tfInt16#  :: Int16#
  , tfWord8#  :: Word8#
  , tfWord16# :: Word16#
# if MIN_VERSION_base(4,16,0)
  , tfInt32#  :: Int32#
  , tfWord32# :: Word32#
# endif
} deriving Show
#endif

-------------------------------------------------------------------------------

instance (Arbitrary a, Arbitrary b) => Arbitrary (TyCon# a b) where
    arbitrary = genericArbitrary

instance (Arbitrary a, Arbitrary b) => Arbitrary (TyFamily# a b) where
    arbitrary = genericArbitrary

#if MIN_VERSION_base(4,13,0)
instance (Arbitrary a, Arbitrary b) => Arbitrary (TyCon'# a b) where
    arbitrary = do
      a     <- arbitrary
      b     <- arbitrary
      I# i1 <- arbitrary
      I# i2 <- arbitrary
      W# w1 <- arbitrary
      W# w2 <- arbitrary
# if MIN_VERSION_base(4,16,0)
      I# i3 <- arbitrary
      W# w3 <- arbitrary
# endif
      pure $ TyCon'# a b (intToInt8Compat# i1)   (intToInt16Compat# i2)
                         (wordToWord8Compat# w1) (wordToWord16Compat# w2)
# if MIN_VERSION_base(4,16,0)
                         (intToInt32# i3)        (wordToWord32# w3)
# endif

instance (Arbitrary a, Arbitrary b) => Arbitrary (TyFamily'# a b) where
    arbitrary = do
      a     <- arbitrary
      b     <- arbitrary
      I# i1 <- arbitrary
      I# i2 <- arbitrary
      W# w1 <- arbitrary
      W# w2 <- arbitrary
# if MIN_VERSION_base(4,16,0)
      I# i3 <- arbitrary
      W# w3 <- arbitrary
# endif
      pure $ TyFamily'# a b (intToInt8Compat# i1)   (intToInt16Compat# i2)
                            (wordToWord8Compat# w1) (wordToWord16Compat# w2)
# if MIN_VERSION_base(4,16,0)
                            (intToInt32# i3)        (wordToWord32# w3)
# endif

# if MIN_VERSION_base(4,16,0)
intToInt8Compat# :: Int# -> Int8#
intToInt8Compat# = intToInt8#

intToInt16Compat# :: Int# -> Int16#
intToInt16Compat# = intToInt16#

wordToWord8Compat# :: Word# -> Word8#
wordToWord8Compat# = wordToWord8#

wordToWord16Compat# :: Word# -> Word16#
wordToWord16Compat# = wordToWord16#
# else
intToInt8Compat# :: Int# -> Int8#
intToInt8Compat# = narrowInt8#

intToInt16Compat# :: Int# -> Int16#
intToInt16Compat# = narrowInt16#

wordToWord8Compat# :: Word# -> Word8#
wordToWord8Compat# = narrowWord8#

wordToWord16Compat# :: Word# -> Word16#
wordToWord16Compat# = narrowWord16#
# endif
#endif

-------------------------------------------------------------------------------

$(deriveShow1Options legacyShowOptions ''TyCon#)
#if defined(NEW_FUNCTOR_CLASSES)
$(deriveShow2Options legacyShowOptions ''TyCon#)
#endif

$(deriveTextShow  ''TyCon#)
$(deriveTextShow1 ''TyCon#)
$(deriveTextShow2 ''TyCon#)

#if __GLASGOW_HASKELL__ < 711
$(Generics.deriveAll0And1 ''TyCon#)
#endif

#if !defined(NEW_FUNCTOR_CLASSES)
$(deriveShow1Options legacyShowOptions 'TyFamily#)
#else
$(deriveShow1Options legacyShowOptions 'TyFamily#)
$(deriveShow2Options legacyShowOptions 'TyFamily#)
#endif

$(deriveTextShow  'TyFamily#)
$(deriveTextShow1 'TyFamily#)
$(deriveTextShow2 'TyFamily#)

#if __GLASGOW_HASKELL__ < 711
$(Generics.deriveAll0And1 'TyFamily#)
#endif

#if MIN_VERSION_base(4,13,0)
$(deriveShow1Options legacyShowOptions ''TyCon'#)
$(deriveShow2Options legacyShowOptions ''TyCon'#)
$(deriveTextShow  ''TyCon'#)
$(deriveTextShow1 ''TyCon'#)
$(deriveTextShow2 ''TyCon'#)

$(deriveShow1Options legacyShowOptions 'TyFamily'#)
$(deriveShow2Options legacyShowOptions 'TyFamily'#)
$(deriveTextShow  'TyFamily'#)
$(deriveTextShow1 'TyFamily'#)
$(deriveTextShow2 'TyFamily'#)
#endif
