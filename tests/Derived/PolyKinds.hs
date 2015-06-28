{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE DeriveGeneric              #-}
#endif

#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE PolyKinds                  #-}
#endif

{-|
Module:      Derived.PolyKinds
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Defines data types with poly-kinded type variables.
-}
module Derived.PolyKinds (
      TyConCompose(..)
    , TyConProxy(..)
    , TyFamilyCompose(..)
    , TyFamilyProxy(..)
    ) where

#include "generic.h"

#if __GLASGOW_HASKELL__ >= 702
import GHC.Generics (Generic)
# if defined(__LANGUAGE_DERIVE_GENERIC1__)
import GHC.Generics (Generic1)
# endif
#endif
import GHC.Show (appPrec, appPrec1, showSpace)

import Prelude hiding (Show)

import Test.QuickCheck (Arbitrary)

import Text.Show as S (Show)
import Text.Show.Text as T (Show(..), Show1(..))
import Text.Show.Text.TH (deriveShow, deriveShow1, deriveShow2,
                          mkShowbPrec, mkShowbPrecWith)

import TransformersCompat as S (Show1(..), Show2(..))

-------------------------------------------------------------------------------

newtype TyConCompose f g h j k a b =
    TyConCompose (f (g (j a) (k a)) (h (j a) (k b)))
#if __GLASGOW_HASKELL__ >= 702
  deriving Generic
#endif

deriving instance Arbitrary (f (g (j a) (k a)) (h (j a) (k b))) =>
  Arbitrary (TyConCompose f g h j k a b)

# if defined(__LANGUAGE_DERIVE_GENERIC1__)
deriving instance ( Functor (f (g (j a) (k a)))
                  , Functor (h (j a))
                  , Functor k
                  ) => Generic1 (TyConCompose f g h j k a)
# endif

deriving instance S.Show (f (g (j a) (k a)) (h (j a) (k b))) =>
  S.Show (TyConCompose f g h j k a b)

-------------------------------------------------------------------------------

newtype TyConProxy a b = TyConProxy ()
  deriving ( Arbitrary
           , S.Show
#if __GLASGOW_HASKELL__ >= 702
           , Generic
# if defined(__LANGUAGE_DERIVE_GENERIC1__)
           , Generic1
# endif
#endif
           )

-------------------------------------------------------------------------------

data family TyFamilyCompose
#if __GLASGOW_HASKELL__ >= 708 && __GLASGOW_HASKELL__ < 710
                            (f :: k1 -> k2 -> *)
                            (g :: k3 -> k4 -> k1)
                            (h :: k3 -> k4 -> k2)
                            (j :: k5 -> k3)
                            (k :: k5 -> k4)
                            (a :: k5)
                            (b :: k5)
#elif __GLASGOW_HASKELL__ >= 706
                            (t :: k1 -> k2 -> *)
                            (u :: k3 -> k4 -> k1)
                            (v :: k3 -> k4 -> k2)
                            (w :: k5 -> k3)
                            (x :: k5 -> k4)
                            (y :: k5)
                            (z :: k5)
#else
                            (t :: * -> * -> *)
                            (u :: * -> * -> *)
                            (v :: * -> * -> *)
                            (w :: * -> *)
                            (x :: * -> *)
                            (y :: *)
                            (z :: *)
#endif
                            :: *
newtype instance TyFamilyCompose f g h j k a b =
    TyFamilyCompose (f (g (j a) (k a)) (h (j a) (k b)))
#if __GLASGOW_HASKELL__ >= 706
  deriving Generic
#endif

deriving instance Arbitrary (f (g (j a) (k a)) (h (j a) (k b))) =>
  Arbitrary (TyFamilyCompose f g h j k a b)

# if defined(__LANGUAGE_DERIVE_GENERIC1__)
deriving instance ( Functor (f (g (j a) (k a)))
                  , Functor (h (j a))
                  , Functor k
                  ) => Generic1 (TyFamilyCompose f g h j k a)
# endif

deriving instance S.Show (f (g (j a) (k a)) (h (j a) (k b))) =>
  S.Show (TyFamilyCompose f g h j k a b)

-------------------------------------------------------------------------------

data family TyFamilyProxy
#if __GLASGOW_HASKELL__ >= 708 && __GLASGOW_HASKELL__ < 710
                          (a :: k1) (b :: k2) :: *
#elif __GLASGOW_HASKELL__ >= 706
                          (x :: k1) (y :: k2) :: *
#else
                          (x :: *)  (y :: *)  :: *
#endif


newtype instance TyFamilyProxy a b = TyFamilyProxy ()
  deriving ( Arbitrary
           , S.Show
#if __GLASGOW_HASKELL__ >= 706
           , Generic
# if defined(__LANGUAGE_DERIVE_GENERIC1__)
           , Generic1
# endif
#endif
           )

-------------------------------------------------------------------------------

instance (S.Show1 (f (g (j a) (k a))), S.Show1 (h (j a)), S.Show1 k) =>
  S.Show1 (TyConCompose f g h j k a) where
    showsPrecWith sp p (TyConCompose x) =
        showsPrecCompose sp "TyConCompose" p x
instance (S.Show2 f, S.Show2 g, S.Show2 h, S.Show1 j, S.Show1 k) =>
  S.Show2 (TyConCompose f g h j k) where
    showsPrecWith2 sp1 sp2 p (TyConCompose x) =
        showsPrecCompose2 sp1 sp2 "TyConCompose" p x

instance S.Show1 (TyConProxy (a :: *)) where
    showsPrecWith = showsPrecWith2 undefined
instance S.Show2 TyConProxy where
    showsPrecWith2 _ _ p (TyConProxy x) = showParen (p > appPrec) $
          showString "TyConProxy "
        . showsPrec appPrec1 x

instance (S.Show1 (f (g (j a) (k a))), S.Show1 (h (j a)), S.Show1 k) =>
  S.Show1 (TyFamilyCompose f g h j k a) where
    showsPrecWith sp p (TyFamilyCompose x) =
        showsPrecCompose sp "TyFamilyCompose" p x
instance (S.Show2 f, S.Show2 g, S.Show2 h, S.Show1 j, S.Show1 k) =>
  S.Show2 (TyFamilyCompose f g h j k) where
    showsPrecWith2 sp1 sp2 p (TyFamilyCompose x) =
        showsPrecCompose2 sp1 sp2 "TyFamilyCompose" p x

instance S.Show1 (TyFamilyProxy (a :: *)) where
    showsPrecWith = showsPrecWith2 undefined
instance S.Show2 TyFamilyProxy where
    showsPrecWith2 _ _ p (TyFamilyProxy x) = showParen (p > appPrec) $
          showString "TyFamilyProxy "
        . showsPrec appPrec1 x

showsPrecCompose :: (S.Show1 (f (g (j a) (k a))), S.Show1 (h (j a)), S.Show1 k)
                 => (Int -> b -> ShowS) -> String
                 -> Int -> f (g (j a) (k a)) (h (j a) (k b)) -> ShowS
showsPrecCompose sp name p x = showParen (p > appPrec) $
      showString name . showSpace
    . showsPrecWith (showsPrecWith (showsPrecWith sp)) appPrec1 x

showsPrecCompose2 :: (S.Show2 f, S.Show2 g, S.Show2 h, S.Show1 j, S.Show1 k)
                  => (Int -> a -> ShowS) -> (Int -> b -> ShowS)
                  -> String -> Int -> f (g (j a) (k a)) (h (j a) (k b)) -> ShowS
showsPrecCompose2 sp1 sp2 name p x = showParen (p > appPrec) $
      showString name . showSpace
    . showsPrecWith2 (showsPrecWith2 (showsPrecWith sp1) (showsPrecWith sp1))
                     (showsPrecWith2 (showsPrecWith sp1) (showsPrecWith sp2))
                     appPrec1 x

-------------------------------------------------------------------------------

$(return [])

instance T.Show (f (g (j a) (k a)) (h (j a) (k b))) =>
  T.Show (TyConCompose f g h j k a b) where
    showbPrec = $(mkShowbPrec ''TyConCompose)
instance (T.Show1 (f (g (j a) (k a))), T.Show1 (h (j a)), T.Show1 k) =>
  T.Show1 (TyConCompose f g h j k a) where
    showbPrecWith = $(mkShowbPrecWith ''TyConCompose)
$(deriveShow2 ''TyConCompose)

$(deriveShow  ''TyConProxy)
$(deriveShow1 ''TyConProxy)
$(deriveShow2 ''TyConProxy)

instance T.Show (f (g (j a) (k a)) (h (j a) (k b))) =>
  T.Show (TyFamilyCompose f g h j k a b) where
    showbPrec = $(mkShowbPrec 'TyFamilyCompose)
instance (T.Show1 (f (g (j a) (k a))), T.Show1 (h (j a)), T.Show1 k) =>
  T.Show1 (TyFamilyCompose f g h j k a) where
    showbPrecWith = $(mkShowbPrecWith 'TyFamilyCompose)
$(deriveShow2 'TyFamilyCompose)

$(deriveShow  'TyFamilyProxy)
$(deriveShow1 'TyFamilyProxy)
$(deriveShow2 'TyFamilyProxy)
