{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE DeriveGeneric   #-}
#endif

{-|
Module:      Derived.Records
Copyright:   (C) 2014-2015 Ryan Scott
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
import           GHC.Show (showSpace)
#if __GLASGOW_HASKELL__ < 711
import           GHC.Show (appPrec)
#endif

import           Prelude ()
import           Prelude.Compat

import           Test.QuickCheck (Arbitrary(..), oneof)

import           TextShow.TH (deriveTextShow, deriveTextShow1, deriveTextShow2)

import           TransformersCompat (Show1(..), Show2(..))

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

instance Show a => Show1 (TyCon a) where
    showsPrecWith = showsPrecWith2 showsPrec
instance Show2 TyCon where
    showsPrecWith2 sp1 sp2 p (TyConPrefix a b) =
        showsRecord sp1 sp2 "TyConPrefix" "tc1" "tc2" p a b
    showsPrecWith2 sp1 sp2 p (a :@: b) =
        showsRecord sp2 sp1 "(:@:)" "tc3" "tc4" p a b

instance Show a => Show1 (TyFamily a) where
    showsPrecWith = showsPrecWith2 showsPrec
instance Show2 TyFamily where
    showsPrecWith2 sp1 sp2 p (TyFamilyPrefix a b) =
        showsRecord sp1 sp2 "TyFamilyPrefix" "tf1" "tf2" p a b
    showsPrecWith2 sp1 sp2 p (a :!: b) =
        showsRecord sp2 sp1 "(:!:)" "tf3" "tf4" p a b

showsRecord :: (Int -> a -> ShowS) -> (Int -> b -> ShowS)
            -> String -> String -> String -> Int -> a -> b -> ShowS
showsRecord sp1 sp2 con rec1 rec2 _p a b =
#if __GLASGOW_HASKELL__ < 711
    showParen (_p > appPrec) $
#endif
          showString con . showSpace
        . showChar '{'
        . showString rec1 . showString " = " . sp1 0 a . showString ", "
        . showString rec2 . showString " = " . sp2 0 b
        . showChar '}'

-------------------------------------------------------------------------------

$(deriveTextShow  ''TyCon)
$(deriveTextShow1 ''TyCon)
$(deriveTextShow2 ''TyCon)

#if MIN_VERSION_template_haskell(2,7,0)
$(deriveTextShow  'TyFamilyPrefix)
$(deriveTextShow1 '(:!:))
$(deriveTextShow2 'TyFamilyPrefix)
#endif

#if __GLASGOW_HASKELL__ < 706
$(Generics.deriveMeta           ''TyCon)
$(Generics.deriveRepresentable1 ''TyCon)
#endif

#if __GLASGOW_HASKELL__ < 702
$(Generics.deriveRepresentable0 ''TyCon)
#endif

#if MIN_VERSION_template_haskell(2,7,0)
# if !defined(__LANGUAGE_DERIVE_GENERIC1__)
$(Generics.deriveMeta           'TyFamilyPrefix)
$(Generics.deriveRepresentable1 '(:!:))
# endif

# if __GLASGOW_HASKELL__ < 706
$(Generics.deriveRepresentable0 'TyFamilyPrefix)
# endif
#endif
