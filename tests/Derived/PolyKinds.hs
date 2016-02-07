{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE DeriveGeneric              #-}
#endif

#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE PolyKinds                  #-}
#endif

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module:      Derived.PolyKinds
Copyright:   (C) 2014-2016 Ryan Scott
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
#if !defined(__LANGUAGE_DERIVE_GENERIC1__) || MIN_VERSION_template_haskell(2,7,0)
import qualified Generics.Deriving.TH as Generics
#endif

import           Test.QuickCheck (Arbitrary)

import           TextShow (TextShow(..), TextShow1(..), TextShow2(..))
import           TextShow.TH (deriveTextShow2, makeShowbPrec,
                              makeLiftShowbPrec, makeLiftShowbPrec2)

#if MIN_VERSION_transformers(0,4,0) && !(MIN_VERSION_transformers(0,5,0))
import           Data.Functor.Classes (showsUnary1)
#else
import           Data.Functor.Classes (Show2(..), showsUnaryWith)
import           GHC.Show (appPrec, appPrec1, showSpace)
#endif

-------------------------------------------------------------------------------

newtype TyConCompose f g h j k a b =
    TyConCompose (f (g (j a) (k a)) (h (j a) (k b)))
#if __GLASGOW_HASKELL__ >= 702
  deriving Generic
#endif

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
#endif
           )

-------------------------------------------------------------------------------

#if MIN_VERSION_transformers(0,4,0) && !(MIN_VERSION_transformers(0,5,0))
-- | Kludge to get type with the same instances as @g a@
newtype Apply g a = Apply (g a)

instance (Show1 g, Show a) => Show (Apply g a) where
    showsPrec d (Apply x) = showsPrec1 d x

instance (Functor (f (g (j a) (k a))), Functor (h (j a)),
          Show1 (f (g (j a) (k a))), Show1 (h (j a)), Show1 k) =>
  Show1 (TyConCompose f g h j k a) where
    showsPrec1 p (TyConCompose x) =
      showsUnary1 "TyConCompose" p $ fmap (Apply . fmap Apply) x
instance Show1 (TyConProxy (a :: *)) where
    showsPrec1 = showsPrec
instance Show1 (f a b c d) => Show1 (TyConReallyHighKinds f a b c d) where
    showsPrec1 p (TyConReallyHighKinds x) =
      showsUnary1 "TyConReallyHighKinds" p x
instance (Functor (f (g (j a) (k a))), Functor (h (j a)),
          Show1 (f (g (j a) (k a))), Show1 (h (j a)), Show1 k) =>
  Show1 (TyFamilyCompose f g h j k a) where
    showsPrec1 p (TyFamilyCompose x) =
      showsUnary1 "TyFamilyCompose" p $ fmap (Apply . fmap Apply) x
instance Show1 (TyFamilyProxy (a :: *)) where
    showsPrec1 = showsPrec
instance Show1 (f a b c d) => Show1 (TyFamilyReallyHighKinds f a b c d) where
    showsPrec1 p (TyFamilyReallyHighKinds x) =
      showsUnary1 "TyFamilyReallyHighKinds" p x

-- TODO: Move this to base-orphans someday
instance (Show a, Show b, Show c, Show d) => Show1 ((,,,,) a b c d) where
    showsPrec1 = showsPrec
#else
instance (Show1 (f (g (j a) (k a))), Show1 (h (j a)), Show1 k) =>
  Show1 (TyConCompose f g h j k a) where
    liftShowsPrec sp sl p (TyConCompose x) =
        showsPrecCompose sp sl "TyConCompose" p x
instance Show1 (TyConProxy (a :: *)) where
    liftShowsPrec = liftShowsPrec2 undefined undefined
instance Show1 (f a b c d) => Show1 (TyConReallyHighKinds f a b c d) where
    liftShowsPrec sp sl p (TyConReallyHighKinds x) =
        showsUnaryWith (liftShowsPrec sp sl) "TyConReallyHighKinds" p x
instance (Show1 (f (g (j a) (k a))), Show1 (h (j a)), Show1 k) =>
  Show1 (TyFamilyCompose f g h j k a) where
    liftShowsPrec sp sl p (TyFamilyCompose x) =
        showsPrecCompose sp sl "TyFamilyCompose" p x
instance Show1 (TyFamilyProxy (a :: *)) where
    liftShowsPrec = liftShowsPrec2 undefined undefined
instance Show1 (f a b c d) => Show1 (TyFamilyReallyHighKinds f a b c d) where
    liftShowsPrec sp sl p (TyFamilyReallyHighKinds x) =
        showsUnaryWith (liftShowsPrec sp sl) "TyFamilyReallyHighKinds" p x

instance (Show2 f, Show2 g, Show2 h, Show1 j, Show1 k) =>
  Show2 (TyConCompose f g h j k) where
    liftShowsPrec2 sp1 sl1 sp2 sl2 p (TyConCompose x) =
        showsPrecCompose2 sp1 sl1 sp2 sl2 "TyConCompose" p x
instance Show2 TyConProxy where
    liftShowsPrec2 _ _ _ _ p (TyConProxy x) = showParen (p > appPrec) $
          showString "TyConProxy "
        . showsPrec appPrec1 x
instance Show2 (f a b c) => Show2 (TyConReallyHighKinds f a b c) where
    liftShowsPrec2 sp1 sl1 sp2 sl2 p (TyConReallyHighKinds x) =
        showsUnaryWith (liftShowsPrec2 sp1 sl1 sp2 sl2) "TyConReallyHighKinds" p x
instance (Show2 f, Show2 g, Show2 h, Show1 j, Show1 k) =>
  Show2 (TyFamilyCompose f g h j k) where
    liftShowsPrec2 sp1 sl1 sp2 sl2 p (TyFamilyCompose x) =
        showsPrecCompose2 sp1 sl1 sp2 sl2 "TyFamilyCompose" p x
instance Show2 TyFamilyProxy where
    liftShowsPrec2 _ _ _ _ p (TyFamilyProxy x) = showParen (p > appPrec) $
          showString "TyFamilyProxy "
        . showsPrec appPrec1 x
instance Show2 (f a b c) => Show2 (TyFamilyReallyHighKinds f a b c) where
    liftShowsPrec2 sp1 sl1 sp2 sl2 p (TyFamilyReallyHighKinds x) =
        showsUnaryWith (liftShowsPrec2 sp1 sl1 sp2 sl2) "TyFamilyReallyHighKinds" p x

showsPrecCompose :: (Show1 (f (g (j a) (k a))), Show1 (h (j a)), Show1 k)
                 => (Int -> b -> ShowS) -> ([b] -> ShowS)
                 -> String -> Int -> f (g (j a) (k a)) (h (j a) (k b)) -> ShowS
showsPrecCompose sp sl name p x = showParen (p > appPrec) $
      showString name . showSpace
    . liftShowsPrec (liftShowsPrec (liftShowsPrec sp sl)
                                   (liftShowList  sp sl))
                    (liftShowList  (liftShowsPrec sp sl)
                                   (liftShowList  sp sl))
                    appPrec1 x

showsPrecCompose2 :: (Show2 f, Show2 g, Show2 h, Show1 j, Show1 k)
                  => (Int -> a -> ShowS) -> ([a] -> ShowS)
                  -> (Int -> b -> ShowS) -> ([b] -> ShowS)
                  -> String -> Int -> f (g (j a) (k a)) (h (j a) (k b)) -> ShowS
showsPrecCompose2 sp1 sl1 sp2 sl2 name p x = showParen (p > appPrec) $
      showString name . showSpace
    . liftShowsPrec2 (liftShowsPrec2 (liftShowsPrec sp1 sl1)
                                     (liftShowList  sp1 sl1)
                                     (liftShowsPrec sp1 sl1)
                                     (liftShowList  sp1 sl1))
                     (liftShowList2  (liftShowsPrec sp1 sl1)
                                     (liftShowList  sp1 sl1)
                                     (liftShowsPrec sp1 sl1)
                                     (liftShowList  sp1 sl1))
                     (liftShowsPrec2 (liftShowsPrec sp1 sl1)
                                     (liftShowList  sp1 sl1)
                                     (liftShowsPrec sp2 sl2)
                                     (liftShowList  sp2 sl2))
                     (liftShowList2  (liftShowsPrec sp1 sl1)
                                     (liftShowList  sp1 sl1)
                                     (liftShowsPrec sp2 sl2)
                                     (liftShowList  sp2 sl2))
                     appPrec1 x

-- TODO: Move these to base-orphans someday
instance (Show a, Show b, Show c, Show d) => Show1 ((,,,,) a b c d) where
    liftShowsPrec = liftShowsPrec2 showsPrec showList
instance (Show a, Show b, Show c) => Show2 ((,,,,) a b c) where
    liftShowsPrec2 sp1 _ sp2 _ _ (a, b, c, d, e) =
        showChar '(' . shows a . showChar ','
                     . shows b . showChar ','
                     . shows c . showChar ','
                     . sp1 0 d . showChar ','
                     . sp2 0 e . showChar ')'
#endif

-------------------------------------------------------------------------------

$(return [])

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

#if MIN_VERSION_template_haskell(2,7,0)
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
#endif

#if !defined(__LANGUAGE_DERIVE_GENERIC1__)
$(Generics.deriveMeta           ''TyConCompose)
$(Generics.deriveRep1           ''TyConCompose)

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

#if __GLASGOW_HASKELL__ < 702
$(Generics.deriveRepresentable0 ''TyConCompose)
$(Generics.deriveRepresentable0 ''TyConProxy)
$(Generics.deriveRepresentable0 ''TyConReallyHighKinds)
#endif

#if MIN_VERSION_template_haskell(2,7,0)
-- TODO: Reinstate CPP bounds once Trac #11357 is fixed
$(Generics.deriveMeta           'TyFamilyCompose)
$(Generics.deriveRep1           'TyFamilyCompose)

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

# if __GLASGOW_HASKELL__ < 706
$(Generics.deriveRepresentable0 'TyFamilyCompose)
$(Generics.deriveRepresentable0 'TyFamilyProxy)
$(Generics.deriveRepresentable0 'TyFamilyReallyHighKinds)
# endif
#endif
