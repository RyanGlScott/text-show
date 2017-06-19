{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE PolyKinds                  #-}
#endif

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module:      Derived.PolyKinds
Copyright:   (C) 2014-2017 Ryan Scott
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

import           Data.Functor.Classes (Show1(..))

import           Generics.Deriving.Base
#if !defined(__LANGUAGE_DERIVE_GENERIC1__)
import qualified Generics.Deriving.TH as Generics
#endif

import           Test.QuickCheck (Arbitrary)

import           Text.Show.Deriving (deriveShow1)
import           TextShow (TextShow(..), TextShow1(..), TextShow2(..))
import           TextShow.TH (deriveTextShow2, makeShowbPrec,
                              makeLiftShowbPrec, makeLiftShowbPrec2)

#if defined(NEW_FUNCTOR_CLASSES)
import           Data.Functor.Classes (Show2(..))
import           Text.Show.Deriving (deriveShow2, makeLiftShowsPrec, makeLiftShowsPrec2)
#else
import           Text.Show.Deriving (makeShowsPrec1)
#endif

-------------------------------------------------------------------------------

-- NB: Don't use k as a type variable here! It'll trigger GHC Trac #12503.
newtype TyConCompose f g h j p a b =
    TyConCompose (f (g (j a) (p a)) (h (j a) (p b)))
  deriving Generic

deriving instance Arbitrary (f (g (j a) (k a)) (h (j a) (k b))) =>
  Arbitrary (TyConCompose f g h j k a b)

#if defined(__LANGUAGE_DERIVE_GENERIC1__)
deriving instance ( Functor (f (g (j a) (k a)))
                  , Functor (h (j a))
                  ) => Generic1 (TyConCompose f g h j k a)
#endif

deriving instance Show (f (g (j a) (k a)) (h (j a) (k b))) =>
  Show (TyConCompose f g h j k a b)

-------------------------------------------------------------------------------

newtype TyConProxy a b where
    TyConProxy :: () -> TyConProxy a b
  deriving ( Arbitrary
           , Show
           , Generic
#if defined(__LANGUAGE_DERIVE_GENERIC1__)
           , Generic1
#endif
           )

-------------------------------------------------------------------------------

newtype TyConReallyHighKinds f a b c d e = TyConReallyHighKinds (f a b c d e)
  deriving ( Arbitrary
           , Show
           , Generic
#if defined(__LANGUAGE_DERIVE_GENERIC1__)
           , Generic1
#endif
           )

-------------------------------------------------------------------------------

data family TyFamilyCompose
#if __GLASGOW_HASKELL__ >= 706
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

#if defined(__LANGUAGE_DERIVE_GENERIC1__)
deriving instance ( Functor (f (g (j a) (k a)))
                  , Functor (h (j a))
                  ) => Generic1 (TyFamilyCompose f g h j k a)
#endif

deriving instance Show (f (g (j a) (k a)) (h (j a) (k b))) =>
  Show (TyFamilyCompose f g h j k a b)

-------------------------------------------------------------------------------

data family TyFamilyProxy
#if __GLASGOW_HASKELL__ >= 706
    (x :: k1) (y :: k2)
#else
    (x :: *)  (y :: *)
#endif
    :: *

newtype instance TyFamilyProxy a b where
    TyFamilyProxy :: () -> TyFamilyProxy a b
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
#if __GLASGOW_HASKELL__ >= 706
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

$(return [])

-- TODO: Replace these with non-orphan instances
$(deriveShow1 ''(,,,,))
#if defined(NEW_FUNCTOR_CLASSES)
$(deriveShow2 ''(,,,,))
#endif

#if defined(NEW_FUNCTOR_CLASSES)
instance (Show1 (f (g (j a) (k a))), Show1 (h (j a)), Show1 k) =>
  Show1 (TyConCompose f g h j k a) where
    liftShowsPrec = $(makeLiftShowsPrec ''TyConCompose)
instance (Show2 f, Show2 g, Show2 h, Show1 j, Show1 k) =>
  Show2 (TyConCompose f g h j k) where
    liftShowsPrec2 = $(makeLiftShowsPrec2 ''TyConCompose)

instance Show1 (TyConProxy (a :: *)) where
    liftShowsPrec = $(makeLiftShowsPrec ''TyConProxy)
instance Show2 TyConProxy where
    liftShowsPrec2 = $(makeLiftShowsPrec2 ''TyConProxy)

instance Show1 (f a b c d) => Show1 (TyConReallyHighKinds f a b c d) where
    liftShowsPrec = $(makeLiftShowsPrec ''TyConReallyHighKinds)
instance Show2 (f a b c) => Show2 (TyConReallyHighKinds f a b c) where
    liftShowsPrec2 = $(makeLiftShowsPrec2 ''TyConReallyHighKinds)
#else
instance (Functor (f (g (j a) (k a))), Functor (h (j a)),
          Show1 (f (g (j a) (k a))), Show1 (h (j a)), Show1 k) =>
  Show1 (TyConCompose f g h j k a) where
    showsPrec1 = $(makeShowsPrec1 ''TyConCompose)

instance Show1 (TyConProxy (a :: *)) where
    showsPrec1 = $(makeShowsPrec1 ''TyConProxy)

instance Show1 (f a b c d) => Show1 (TyConReallyHighKinds f a b c d) where
    showsPrec1 = $(makeShowsPrec1 ''TyConReallyHighKinds)
#endif

instance TextShow (f (g (j a) (k a)) (h (j a) (k b))) =>
  TextShow (TyConCompose f g h j k a b) where
    showbPrec = $(makeShowbPrec ''TyConCompose)
instance (TextShow1 (f (g (j a) (k a))), TextShow1 (h (j a)), TextShow1 k) =>
  TextShow1 (TyConCompose f g h j k a) where
    liftShowbPrec = $(makeLiftShowbPrec ''TyConCompose)
$(deriveTextShow2 ''TyConCompose)

instance TextShow (TyConProxy a b) where
    showbPrec = $(makeShowbPrec ''TyConProxy)
instance TextShow1 (TyConProxy a) where
    liftShowbPrec = $(makeLiftShowbPrec ''TyConProxy)
$(deriveTextShow2 ''TyConProxy)

instance TextShow (f a b c d e) => TextShow (TyConReallyHighKinds f a b c d e) where
    showbPrec = $(makeShowbPrec ''TyConReallyHighKinds)
instance TextShow1 (f a b c d) => TextShow1 (TyConReallyHighKinds f a b c d) where
    liftShowbPrec = $(makeLiftShowbPrec ''TyConReallyHighKinds)
instance TextShow2 (f a b c) => TextShow2 (TyConReallyHighKinds f a b c) where
    liftShowbPrec2 = $(makeLiftShowbPrec2 ''TyConReallyHighKinds)

#if !defined(__LANGUAGE_DERIVE_GENERIC1__)
$(Generics.deriveMeta              ''TyConCompose)
$(Generics.deriveRep1Options False ''TyConCompose)

instance ( Functor (f (g (j a) (k a)))
         , Functor (h (j a))
         ) => Generic1 (TyConCompose f g h j k a) where
    type Rep1 (TyConCompose f g h j k a) = $(Generics.makeRep1 ''TyConCompose) f g h j k a
    from1 = $(Generics.makeFrom1 ''TyConCompose)
    to1   = $(Generics.makeTo1   ''TyConCompose)

$(Generics.deriveMeta           ''TyConProxy)
$(Generics.deriveRepresentable1 ''TyConProxy)
$(Generics.deriveMeta           ''TyConReallyHighKinds)
$(Generics.deriveRepresentable1 ''TyConReallyHighKinds)
#endif

#if !defined(NEW_FUNCTOR_CLASSES)
instance (Functor (f (g (j a) (k a))), Functor (h (j a)),
          Show1 (f (g (j a) (k a))), Show1 (h (j a)), Show1 k) =>
  Show1 (TyFamilyCompose f g h j k a) where
    showsPrec1 = $(makeShowsPrec1 'TyFamilyCompose)
instance Show1 (TyFamilyProxy (a :: *)) where
    showsPrec1 = $(makeShowsPrec1 'TyFamilyProxy)
instance Show1 (f a b c d) => Show1 (TyFamilyReallyHighKinds f a b c d) where
    showsPrec1 = $(makeShowsPrec1 'TyFamilyReallyHighKinds)
#else
instance (Show1 (f (g (j a) (k a))), Show1 (h (j a)), Show1 k) =>
  Show1 (TyFamilyCompose f g h j k a) where
    liftShowsPrec = $(makeLiftShowsPrec 'TyFamilyCompose)
instance Show1 (TyFamilyProxy (a :: *)) where
    liftShowsPrec = $(makeLiftShowsPrec 'TyFamilyProxy)
instance Show1 (f a b c d) => Show1 (TyFamilyReallyHighKinds f a b c d) where
    liftShowsPrec = $(makeLiftShowsPrec 'TyFamilyReallyHighKinds)

instance (Show2 f, Show2 g, Show2 h, Show1 j, Show1 k) =>
  Show2 (TyFamilyCompose f g h j k) where
    liftShowsPrec2 = $(makeLiftShowsPrec2 'TyFamilyCompose)
instance Show2 TyFamilyProxy where
    liftShowsPrec2 = $(makeLiftShowsPrec2 'TyFamilyProxy)
instance Show2 (f a b c) => Show2 (TyFamilyReallyHighKinds f a b c) where
    liftShowsPrec2 = $(makeLiftShowsPrec2 'TyFamilyReallyHighKinds)
#endif

instance TextShow (f (g (j a) (k a)) (h (j a) (k b))) =>
  TextShow (TyFamilyCompose f g h j k a b) where
    showbPrec = $(makeShowbPrec 'TyFamilyCompose)
instance (TextShow1 (f (g (j a) (k a))), TextShow1 (h (j a)), TextShow1 k) =>
  TextShow1 (TyFamilyCompose f g h j k a) where
    liftShowbPrec = $(makeLiftShowbPrec 'TyFamilyCompose)
$(deriveTextShow2 'TyFamilyCompose)

instance TextShow (TyFamilyProxy a b) where
    showbPrec = $(makeShowbPrec 'TyFamilyProxy)
instance TextShow1 (TyFamilyProxy a) where
    liftShowbPrec = $(makeLiftShowbPrec 'TyFamilyProxy)
$(deriveTextShow2 'TyFamilyProxy)

instance TextShow (f a b c d e) => TextShow (TyFamilyReallyHighKinds f a b c d e) where
    showbPrec = $(makeShowbPrec 'TyFamilyReallyHighKinds)
instance TextShow1 (f a b c d) => TextShow1 (TyFamilyReallyHighKinds f a b c d) where
    liftShowbPrec = $(makeLiftShowbPrec 'TyFamilyReallyHighKinds)
instance TextShow2 (f a b c) => TextShow2 (TyFamilyReallyHighKinds f a b c) where
    liftShowbPrec2 = $(makeLiftShowbPrec2 'TyFamilyReallyHighKinds)

#if !defined(__LANGUAGE_DERIVE_GENERIC1__)
$(Generics.deriveMeta              'TyFamilyCompose)
$(Generics.deriveRep1Options False 'TyFamilyCompose)

instance ( Functor (f (g (j a) (k a)))
         , Functor (h (j a))
         ) => Generic1 (TyFamilyCompose f g h j k a) where
    type Rep1 (TyFamilyCompose f g h j k a) = $(Generics.makeRep1 'TyFamilyCompose) f g h j k a
    from1 = $(Generics.makeFrom1 'TyFamilyCompose)
    to1   = $(Generics.makeTo1   'TyFamilyCompose)

$(Generics.deriveMeta           'TyFamilyProxy)
$(Generics.deriveRepresentable1 'TyFamilyProxy)
$(Generics.deriveMeta           'TyFamilyReallyHighKinds)
$(Generics.deriveRepresentable1 'TyFamilyReallyHighKinds)
#endif

#if __GLASGOW_HASKELL__ < 706
$(Generics.deriveRepresentable0 'TyFamilyCompose)
$(Generics.deriveRepresentable0 'TyFamilyProxy)
$(Generics.deriveRepresentable0 'TyFamilyReallyHighKinds)
#endif
