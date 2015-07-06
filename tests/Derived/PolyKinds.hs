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
    , TyConReallyHighKinds
    , TyFamilyCompose(..)
    , TyFamilyProxy(..)
    , TyFamilyReallyHighKinds(..)
    ) where

#include "generic.h"

#if __GLASGOW_HASKELL__ >= 702
import           GHC.Generics (Generic)
# if defined(__LANGUAGE_DERIVE_GENERIC1__)
import           GHC.Generics (Generic1)
# endif
#else
import qualified Generics.Deriving.TH as Generics (deriveAll)
#endif
import           GHC.Show (appPrec, appPrec1, showSpace)

import           Test.QuickCheck (Arbitrary)

import           TextShow (TextShow(..), TextShow1(..), TextShow2(..))
import           TextShow.TH (deriveTextShow2, makeShowbPrec,
                              makeShowbPrecWith, makeShowbPrecWith2)

import           TransformersCompat (Show1(..), Show2(..), showsUnaryWith)

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
                  ) => Generic1 (TyConCompose f g h j k a)
# endif

deriving instance Show (f (g (j a) (k a)) (h (j a) (k b))) =>
  Show (TyConCompose f g h j k a b)

-------------------------------------------------------------------------------

newtype TyConProxy a b = TyConProxy ()
  deriving ( Arbitrary
           , Show
#if __GLASGOW_HASKELL__ >= 702
           , Generic
# if defined(__LANGUAGE_DERIVE_GENERIC1__)
           , Generic1
# endif
#endif
           )

-------------------------------------------------------------------------------

newtype TyConReallyHighKinds f a b c d e = TyConReallyHighKinds (f a b c d e)
  deriving ( Arbitrary
           , Show
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
                  ) => Generic1 (TyFamilyCompose f g h j k a)
# endif

deriving instance Show (f (g (j a) (k a)) (h (j a) (k b))) =>
  Show (TyFamilyCompose f g h j k a b)

-------------------------------------------------------------------------------

data family TyFamilyProxy
#if __GLASGOW_HASKELL__ >= 708 && __GLASGOW_HASKELL__ < 710
    (a :: k1) (b :: k2)
#elif __GLASGOW_HASKELL__ >= 706
    (x :: k1) (y :: k2)
#else
    (x :: *)  (y :: *)
#endif
    :: *

newtype instance TyFamilyProxy a b = TyFamilyProxy ()
  deriving ( Arbitrary
           , Show
#if __GLASGOW_HASKELL__ >= 706
           , Generic
# if defined(__LANGUAGE_DERIVE_GENERIC1__)
           , Generic1
# endif
#endif
           )

-------------------------------------------------------------------------------

data family TyFamilyReallyHighKinds
#if __GLASGOW_HASKELL__ >= 708 && __GLASGOW_HASKELL__ < 710
    (f :: k1 -> k2 -> k3 -> k4 -> k5 -> *)
    (a :: k1)
    (b :: k2)
    (c :: k3)
    (d :: k4)
    (e :: k5)
#elif __GLASGOW_HASKELL__ >= 706
    (g :: k1 -> k2 -> k3 -> k4 -> k5 -> *)
    (v :: k1)
    (w :: k2)
    (x :: k3)
    (y :: k4)
    (z :: k5)
#else
    (g :: * -> * -> * -> * -> * -> *)
    (v :: *)
    (w :: *)
    (x :: *)
    (y :: *)
    (z :: *)
#endif
    :: *

newtype instance TyFamilyReallyHighKinds f a b c d e =
    TyFamilyReallyHighKinds (f a b c d e)
  deriving ( Arbitrary
           , Show
#if __GLASGOW_HASKELL__ >= 706
           , Generic
# if defined(__LANGUAGE_DERIVE_GENERIC1__)
           , Generic1
# endif
#endif
           )

-------------------------------------------------------------------------------

instance (Show1 (f (g (j a) (k a))), Show1 (h (j a)), Show1 k) =>
  Show1 (TyConCompose f g h j k a) where
    showsPrecWith sp p (TyConCompose x) =
        showsPrecCompose sp "TyConCompose" p x
instance (Show2 f, Show2 g, Show2 h, Show1 j, Show1 k) =>
  Show2 (TyConCompose f g h j k) where
    showsPrecWith2 sp1 sp2 p (TyConCompose x) =
        showsPrecCompose2 sp1 sp2 "TyConCompose" p x

instance Show1 (TyConProxy (a :: *)) where
    showsPrecWith = showsPrecWith2 undefined
instance Show2 TyConProxy where
    showsPrecWith2 _ _ p (TyConProxy x) = showParen (p > appPrec) $
          showString "TyConProxy "
        . showsPrec appPrec1 x

instance Show1 (f a b c d) => Show1 (TyConReallyHighKinds f a b c d) where
    showsPrecWith sp p (TyConReallyHighKinds x) =
        showsUnaryWith (showsPrecWith sp) "TyConReallyHighKinds" p x
instance Show2 (f a b c) => Show2 (TyConReallyHighKinds f a b c) where
    showsPrecWith2 sp1 sp2 p (TyConReallyHighKinds x) =
        showsUnaryWith (showsPrecWith2 sp1 sp2) "TyConReallyHighKinds" p x

instance (Show1 (f (g (j a) (k a))), Show1 (h (j a)), Show1 k) =>
  Show1 (TyFamilyCompose f g h j k a) where
    showsPrecWith sp p (TyFamilyCompose x) =
        showsPrecCompose sp "TyFamilyCompose" p x
instance (Show2 f, Show2 g, Show2 h, Show1 j, Show1 k) =>
  Show2 (TyFamilyCompose f g h j k) where
    showsPrecWith2 sp1 sp2 p (TyFamilyCompose x) =
        showsPrecCompose2 sp1 sp2 "TyFamilyCompose" p x

instance Show1 (TyFamilyProxy (a :: *)) where
    showsPrecWith = showsPrecWith2 undefined
instance Show2 TyFamilyProxy where
    showsPrecWith2 _ _ p (TyFamilyProxy x) = showParen (p > appPrec) $
          showString "TyFamilyProxy "
        . showsPrec appPrec1 x

instance Show1 (f a b c d) => Show1 (TyFamilyReallyHighKinds f a b c d) where
    showsPrecWith sp p (TyFamilyReallyHighKinds x) =
        showsUnaryWith (showsPrecWith sp) "TyFamilyReallyHighKinds" p x
instance Show2 (f a b c) => Show2 (TyFamilyReallyHighKinds f a b c) where
    showsPrecWith2 sp1 sp2 p (TyFamilyReallyHighKinds x) =
        showsUnaryWith (showsPrecWith2 sp1 sp2) "TyFamilyReallyHighKinds" p x

showsPrecCompose :: (Show1 (f (g (j a) (k a))), Show1 (h (j a)), Show1 k)
                 => (Int -> b -> ShowS) -> String
                 -> Int -> f (g (j a) (k a)) (h (j a) (k b)) -> ShowS
showsPrecCompose sp name p x = showParen (p > appPrec) $
      showString name . showSpace
    . showsPrecWith (showsPrecWith (showsPrecWith sp)) appPrec1 x

showsPrecCompose2 :: (Show2 f, Show2 g, Show2 h, Show1 j, Show1 k)
                  => (Int -> a -> ShowS) -> (Int -> b -> ShowS)
                  -> String -> Int -> f (g (j a) (k a)) (h (j a) (k b)) -> ShowS
showsPrecCompose2 sp1 sp2 name p x = showParen (p > appPrec) $
      showString name . showSpace
    . showsPrecWith2 (showsPrecWith2 (showsPrecWith sp1) (showsPrecWith sp1))
                     (showsPrecWith2 (showsPrecWith sp1) (showsPrecWith sp2))
                     appPrec1 x

-------------------------------------------------------------------------------

$(return [])

instance TextShow (f (g (j a) (k a)) (h (j a) (k b))) =>
  TextShow (TyConCompose f g h j k a b) where
    showbPrec = $(makeShowbPrec ''TyConCompose)
instance (TextShow1 (f (g (j a) (k a))), TextShow1 (h (j a)), TextShow1 k) =>
  TextShow1 (TyConCompose f g h j k a) where
    showbPrecWith = $(makeShowbPrecWith ''TyConCompose)
$(deriveTextShow2 ''TyConCompose)

instance TextShow (TyConProxy a b) where
    showbPrec = $(makeShowbPrec ''TyConProxy)
instance TextShow1 (TyConProxy a) where
    showbPrecWith = $(makeShowbPrecWith ''TyConProxy)
$(deriveTextShow2 ''TyConProxy)

instance TextShow (f a b c d e) => TextShow (TyConReallyHighKinds f a b c d e) where
    showbPrec = $(makeShowbPrec ''TyConReallyHighKinds)
instance TextShow1 (f a b c d) => TextShow1 (TyConReallyHighKinds f a b c d) where
    showbPrecWith = $(makeShowbPrecWith ''TyConReallyHighKinds)
instance TextShow2 (f a b c) => TextShow2 (TyConReallyHighKinds f a b c) where
    showbPrecWith2 = $(makeShowbPrecWith2 ''TyConReallyHighKinds)

#if MIN_VERSION_template_haskell(2,7,0)
instance TextShow (f (g (j a) (k a)) (h (j a) (k b))) =>
  TextShow (TyFamilyCompose f g h j k a b) where
    showbPrec = $(makeShowbPrec 'TyFamilyCompose)
instance (TextShow1 (f (g (j a) (k a))), TextShow1 (h (j a)), TextShow1 k) =>
  TextShow1 (TyFamilyCompose f g h j k a) where
    showbPrecWith = $(makeShowbPrecWith 'TyFamilyCompose)
$(deriveTextShow2 'TyFamilyCompose)

instance TextShow (TyFamilyProxy a b) where
    showbPrec = $(makeShowbPrec 'TyFamilyProxy)
instance TextShow1 (TyFamilyProxy a) where
    showbPrecWith = $(makeShowbPrecWith 'TyFamilyProxy)
$(deriveTextShow2 'TyFamilyProxy)

instance TextShow (f a b c d e) => TextShow (TyFamilyReallyHighKinds f a b c d e) where
    showbPrec = $(makeShowbPrec 'TyFamilyReallyHighKinds)
instance TextShow1 (f a b c d) => TextShow1 (TyFamilyReallyHighKinds f a b c d) where
    showbPrecWith = $(makeShowbPrecWith 'TyFamilyReallyHighKinds)
instance TextShow2 (f a b c) => TextShow2 (TyFamilyReallyHighKinds f a b c) where
    showbPrecWith2 = $(makeShowbPrecWith2 'TyFamilyReallyHighKinds)
#endif

#if __GLASGOW_HASKELL__ < 702
$(Generics.deriveAll ''TyConCompose)
$(Generics.deriveAll ''TyConProxy)
$(Generics.deriveAll ''TyConReallyHighKinds)
#endif
