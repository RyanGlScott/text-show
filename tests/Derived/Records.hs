{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE DeriveGeneric   #-}
#endif

{-|
Module:      Derived.Records
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Defines data types with record syntax.
-}
module Derived.Records (TyCon(..), TyFamily(..)) where

#include "generic.h"

#if !defined(__LANGUAGE_DERIVE_GENERIC1__)
import qualified Generics.Deriving.TH as Generics
#endif

#if __GLASGOW_HASKELL__ >= 702
import           GHC.Generics (Generic)
# if __GLASGOW_HASKELL__ >= 706
import           GHC.Generics (Generic1)
# endif
#endif

import           Prelude ()
import           Prelude.Compat

import           Test.QuickCheck (Arbitrary(..), oneof)

import           Text.Show.Deriving (deriveShow1)
import           TextShow.TH (deriveTextShow, deriveTextShow1, deriveTextShow2)

#if defined(NEW_FUNCTOR_CLASSES)
import           Text.Show.Deriving (deriveShow2)
# if !(MIN_VERSION_template_haskell(2,7,0))
import           Data.Functor.Classes (Show1(..), Show2(..))
import           GHC.Show (showSpace)
import           GHC.Show (appPrec)
# endif
#endif

-------------------------------------------------------------------------------

infixl 4 :@:
data TyCon a b = TyConPrefix { tc1 :: a, tc2 :: b }
               | (:@:)       { tc3 :: b, tc4 :: a }
  deriving ( Show
#if __GLASGOW_HASKELL__ >= 702
           , Generic
# if __GLASGOW_HASKELL__ >= 706
           , Generic1
# endif
#endif
           )

-------------------------------------------------------------------------------

data family TyFamily y z :: *

infixl 4 :!:
data instance TyFamily a b = TyFamilyPrefix { tf1 :: a, tf2 :: b }
                           | (:!:)          { tf3 :: b, tf4 :: a }
  deriving ( Show
#if __GLASGOW_HASKELL__ >= 706
           , Generic
# if defined(__LANGUAGE_DERIVE_GENERIC1__)
           , Generic1
# endif
#endif
           )

-------------------------------------------------------------------------------

instance (Arbitrary a, Arbitrary b) => Arbitrary (TyCon a b) where
    arbitrary = oneof [ TyConPrefix <$> arbitrary <*> arbitrary
                      , (:@:)       <$> arbitrary <*> arbitrary
                      ]

instance (Arbitrary a, Arbitrary b) => Arbitrary (TyFamily a b) where
    arbitrary = oneof [ TyFamilyPrefix <$> arbitrary <*> arbitrary
                      , (:!:)          <$> arbitrary <*> arbitrary
                      ]

-------------------------------------------------------------------------------

$(deriveShow1 ''TyCon)
#if defined(NEW_FUNCTOR_CLASSES)
$(deriveShow2 ''TyCon)
#endif

$(deriveTextShow  ''TyCon)
$(deriveTextShow1 ''TyCon)
$(deriveTextShow2 ''TyCon)

#if __GLASGOW_HASKELL__ < 706
$(Generics.deriveMeta           ''TyCon)
$(Generics.deriveRepresentable1 ''TyCon)
#endif

#if __GLASGOW_HASKELL__ < 702
$(Generics.deriveRepresentable0 ''TyCon)
#endif

#if !defined(NEW_FUNCTOR_CLASSES)
$(deriveShow1 'TyFamilyPrefix)
#elif MIN_VERSION_template_haskell(2,7,0)
$(deriveShow1 'TyFamilyPrefix)
$(deriveShow2 '(:!:))
#else
instance Show a => Show1 (TyFamily a) where
    liftShowsPrec = liftShowsPrec2 showsPrec showList

instance Show2 TyFamily where
    liftShowsPrec2 sp1 _ sp2 _ p (TyFamilyPrefix a b) =
        showsRecord sp1 sp2 "TyFamilyPrefix" "tf1" "tf2" p a b
    liftShowsPrec2 sp1 _ sp2 _ p (a :!: b) =
        showsRecord sp2 sp1 "(:!:)" "tf3" "tf4" p a b

showsRecord :: (Int -> a -> ShowS) -> (Int -> b -> ShowS)
            -> String -> String -> String -> Int -> a -> b -> ShowS
showsRecord sp1 sp2 con rec1 rec2 p a b =
    showParen (p > appPrec) $
          showString con . showSpace
        . showChar '{'
        . showString rec1 . showString " = " . sp1 0 a . showString ", "
        . showString rec2 . showString " = " . sp2 0 b
        . showChar '}'
#endif

#if MIN_VERSION_template_haskell(2,7,0)
$(deriveTextShow  'TyFamilyPrefix)
$(deriveTextShow1 '(:!:))
$(deriveTextShow2 'TyFamilyPrefix)

# if !defined(__LANGUAGE_DERIVE_GENERIC1__)
$(Generics.deriveMeta           'TyFamilyPrefix)
$(Generics.deriveRepresentable1 '(:!:))
# endif

# if __GLASGOW_HASKELL__ < 706
$(Generics.deriveRepresentable0 'TyFamilyPrefix)
# endif
#endif
