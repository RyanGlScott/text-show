{-# LANGUAGE CPP             #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE DeriveGeneric   #-}
#endif

{-|
Module:      Derived.MagicHash
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Defines data types with fields that have unlifted types.
-}
module Derived.MagicHash (TyCon#(..), TyFamily#(..)) where

#if __GLASGOW_HASKELL__ < 711
import qualified Generics.Deriving.TH as Generics
#endif

import           GHC.Exts
#if __GLASGOW_HASKELL__ >= 711
import           GHC.Generics (Generic, Generic1)
#endif

import           Prelude ()
import           Prelude.Compat

import           Test.QuickCheck (Arbitrary(..))

import           Text.Show.Deriving (deriveShow1Options, legacyOptions)
import           TextShow.TH (deriveTextShow, deriveTextShow1, deriveTextShow2)

#if defined(NEW_FUNCTOR_CLASSES)
# if MIN_VERSION_template_haskell(2,7,0)
import           Text.Show.Deriving (deriveShow2Options)
# else
import           Data.Functor.Classes (Show1(..), Show2(..))
import           GHC.Show (showSpace)
import           GHC.Show (appPrec)
# endif
#endif

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

-------------------------------------------------------------------------------

instance (Arbitrary a, Arbitrary b) => Arbitrary (TyCon# a b) where
    arbitrary = do
        a     <- arbitrary
        b     <- arbitrary
        I# i# <- arbitrary
        F# f# <- arbitrary
        D# d# <- arbitrary
        C# c# <- arbitrary
        W# w# <- arbitrary
        pure $ TyCon# a b i# f# d# c# w#

instance (Arbitrary a, Arbitrary b) => Arbitrary (TyFamily# a b) where
    arbitrary = do
        a     <- arbitrary
        b     <- arbitrary
        I# i# <- arbitrary
        F# f# <- arbitrary
        D# d# <- arbitrary
        C# c# <- arbitrary
        W# w# <- arbitrary
        pure $ TyFamily# a b i# f# d# c# w#

-------------------------------------------------------------------------------

$(deriveShow1Options legacyOptions ''TyCon#)
#if defined(NEW_FUNCTOR_CLASSES)
$(deriveShow2Options legacyOptions ''TyCon#)
#endif

$(deriveTextShow  ''TyCon#)
$(deriveTextShow1 ''TyCon#)
$(deriveTextShow2 ''TyCon#)

#if !defined(NEW_FUNCTOR_CLASSES)
$(deriveShow1Options legacyOptions 'TyFamily#)
#elif MIN_VERSION_template_haskell(2,7,0)
$(deriveShow1Options legacyOptions 'TyFamily#)
$(deriveShow2Options legacyOptions 'TyFamily#)
#else
instance Show a => Show1 (TyFamily# a) where
    liftShowsPrec = liftShowsPrec2 showsPrec showList

instance Show2 TyFamily# where
    liftShowsPrec2 sp1 _ sp2 _ p (TyFamily# a b i f d c w) =
        showsHash sp1 sp2 "TyFamily#" "tfA" "tfB" "tfInt#" "tfFloat#"
                  "tfDouble#" "tfChar#" "tfWord#" p a b i f d c w

showsHash :: (Int -> a -> ShowS) -> (Int -> b -> ShowS)
          -> String -> String -> String -> String -> String -> String -> String -> String
          -> Int -> a -> b -> Int# -> Float# -> Double# -> Char# -> Word#
          -> ShowS
showsHash sp1 sp2 con rec1 rec2 rec3 rec4 rec5 rec6 rec7 p a b i f d c w =
    showParen (p > appPrec) $
          showString con . showSpace
        . showChar '{'
        . showString rec1 . equals . sp1 0 a                . comma
        . showString rec2 . equals . sp2 0 b                . comma
        . showString rec3 . equals . shows (I# i) . oneHash . comma
        . showString rec4 . equals . shows (F# f) . oneHash . comma
        . showString rec5 . equals . shows (D# d) . twoHash . comma
        . showString rec6 . equals . shows (C# c) . oneHash . comma
        . showString rec7 . equals . shows (W# w) . twoHash
        . showChar '}'
  where
    comma, equals :: ShowS
    comma  = showString ", "
    equals = showString " = "

    oneHash, twoHash :: ShowS
# if __GLASGOW_HASKELL__ >= 711
    oneHash  = showChar '#'
    twoHash  = showString "##"
# else
    oneHash  = id
    twoHash  = id
# endif
#endif

#if MIN_VERSION_template_haskell(2,7,0)
$(deriveTextShow  'TyFamily#)
$(deriveTextShow1 'TyFamily#)
$(deriveTextShow2 'TyFamily#)
#endif

#if __GLASGOW_HASKELL__ < 711
$(Generics.deriveAll0And1 ''TyCon#)
# if MIN_VERSION_template_haskell(2,7,0)
$(Generics.deriveAll0And1 'TyFamily#)
# endif
#endif
