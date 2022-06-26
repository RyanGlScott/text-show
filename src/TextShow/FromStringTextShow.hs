{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE DeriveLift                 #-}
#endif

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module:      TextShow.FromStringTextShow
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

The 'FromStringShow' and 'FromTextShow' data types.
-}
module TextShow.FromStringTextShow (
      FromStringShow(..)
    , FromTextShow(..)
    , FromStringShow1(..)
    , FromTextShow1(..)
    , FromStringShow2(..)
    , FromTextShow2(..)
    ) where

#include "generic.h"

import           Data.Bifunctor.TH (deriveBifunctor, deriveBifoldable,
                                    deriveBitraversable)
import           Data.Coerce (Coercible, coerce)
import           Data.Data (Data, Typeable)
import           Data.Functor.Classes (Show1(..))
import           Data.Text.Lazy.Builder (Builder)

#if !defined(__LANGUAGE_DERIVE_GENERIC1__)
import qualified Generics.Deriving.TH as Generics
#endif

import           GHC.Generics (Generic, Generic1)

import           Language.Haskell.TH.Lift

import           Prelude ()
import           Prelude.Compat

import           Text.ParserCombinators.ReadPrec (ReadPrec)
import           Text.Read (Read(..))

import           TextShow.Classes (TextShow(..), TextShow1(..), TextShow2(..),
                                   showbPrec1, showbPrec2,
                                   showbPrecToShowsPrec, showsPrecToShowbPrec,
                                   showbToShows, showsToShowb)

#if defined(NEW_FUNCTOR_CLASSES)
import           Data.Functor.Classes (Show2(..), showsPrec1, showsPrec2)
#else
import           Text.Show (showListWith)
#endif

-------------------------------------------------------------------------------

-- | An adapter newtype, suitable for @DerivingVia@.
-- The 'TextShow' instance for 'FromStringShow' is based on its @String@
-- 'Show' instance. That is,
--
-- @
-- showbPrec p ('FromStringShow' x) = 'showsToShowb' 'showsPrec' p x
-- @
--
-- /Since: 2/
newtype FromStringShow a = FromStringShow { fromStringShow :: a }
  deriving ( Data
           , Eq
           , Foldable
           , Functor
           , Generic
           , Generic1
#if __GLASGOW_HASKELL__ >= 800
           , Lift
#endif
           , Ord
           , Traversable
           , Typeable
           )

instance Read a => Read (FromStringShow a) where
    readPrec     = coerce (readPrec     :: ReadPrec a)
    readsPrec    = coerce (readsPrec    :: Int -> ReadS a)
    readList     = coerce (readList     :: ReadS [a])
    readListPrec = coerce (readListPrec :: ReadPrec [a])

type ShowBuilderPrec a = Int -> a -> Builder
type ShowStringPrec a = Int -> a -> ShowS

showsPrecToShowbPrec' :: ShowStringPrec a -> ShowBuilderPrec (FromStringShow a)
showsPrecToShowbPrec' sp = \p -> showsPrecToShowbPrec sp p . fromStringShow

showsToShowb' :: (a -> ShowS) -> FromStringShow a -> Builder
showsToShowb' s (FromStringShow x) = showsToShowb s x

instance Show a => TextShow (FromStringShow a) where
    showbPrec p = showsPrecToShowbPrec showsPrec p . fromStringShow
    showb = showsToShowb shows . fromStringShow
    showbList l = showsToShowb showList (coerce l :: [a])

instance Show a => Show (FromStringShow a) where
    showsPrec = coerce (showsPrec :: Int -> a -> ShowS)
    show      = coerce (show      :: a -> String)
    showList  = coerce (showList  :: [a] -> ShowS)

instance Show1 FromStringShow where
#if defined(NEW_FUNCTOR_CLASSES)
    liftShowList  _  sl   = sl   . coerceList
      where
        coerceList :: [FromStringShow a] -> [a]
        coerceList = coerce
    liftShowsPrec sp _ p = sp p . fromStringShow
#else
    showsPrec1 p = showsPrec p . fromStringShow
#endif

#if 0
instance TextShow1 FromStringShow where
    liftShowbPrec sp' _ p =
        showsPrecToShowbPrec (showbPrecToShowsPrec sp') p . fromStringShow

    liftShowbList _ sl' = showsToShowb (showbToShows sl') . coerceList
      where
        coerceList :: [FromStringShow a] -> [a]
        coerceList = coerce
#endif

-------------------------------------------------------------------------------

-- | An adapter newtype, suitable for @DerivingVia@.
-- The @String@ 'Show' instance for 'FromTextShow' is based on its
-- 'TextShow' instance. That is,
--
-- @
-- showsPrec p ('FromTextShow' x) = 'showbToShows' 'showbPrec' p x
-- @
--
-- /Since: 2/
newtype FromTextShow a = FromTextShow { fromTextShow :: a }
  deriving ( Data
           , Eq
           , Foldable
           , Functor
           , Generic
           , Generic1
#if __GLASGOW_HASKELL__ >= 800
           , Lift
#endif
           , Ord
           , TextShow
           , Traversable
           , Typeable
           )

instance Read a => Read (FromTextShow a) where
    readPrec     = coerce (readPrec     :: ReadPrec a)
    readsPrec    = coerce (readsPrec    :: Int -> ReadS a)
    readList     = coerce (readList     :: ReadS [a])
    readListPrec = coerce (readListPrec :: ReadPrec [a])

showbPrecToShowsPrec' :: ShowBuilderPrec a -> ShowStringPrec (FromTextShow a)
showbPrecToShowsPrec' sp = \p -> showbPrecToShowsPrec sp p . fromTextShow

showbToShows' :: (a -> Builder) -> FromTextShow a -> ShowS
showbToShows' s (FromTextShow x) = showbToShows s x

instance TextShow a => Show (FromTextShow a) where
    showsPrec = showbPrecToShowsPrec' showbPrec
    show x = showbToShows' showb x ""
    showList l = showbToShows showbList (coerce l :: [a])

#if 0
instance Show1 FromTextShow where
#if defined(NEW_FUNCTOR_CLASSES)
    liftShowList _ sl = showbToShows (showsToShowb sl) . coerceList
      where
        coerceList :: [FromTextShow a] -> [a]
        coerceList = coerce
    liftShowsPrec sp _ p
      = showbPrecToShowsPrec (showsPrecToShowbPrec sp) p . fromTextShow
#else
    showsPrec1 p
      = showbPrecToShowsPrec (showsPrecToShowbPrec showsPrec) p . fromTextShow
#endif
#endif

instance TextShow1 FromTextShow where
    liftShowbPrec sp' _ p = sp' p . fromTextShow

    liftShowbList _ sl' = sl' . coerceList
      where
        coerceList :: [FromTextShow a] -> [a]
        coerceList = coerce

-------------------------------------------------------------------------------

-- | An adapter newtype, suitable for @DerivingVia@.
-- The 'TextShow1' instance for 'FromStringShow1' is based on its @String@
-- 'Show1' instance. That is,
--
-- @
-- 'liftShowbPrec' sp sl p ('FromStringShow1' x) =
--     'showsPrecToShowbPrec' ('liftShowsPrec' ('showbPrecToShowsPrec' sp)
--                                             ('showbToShows'         sl))
--                            p x
-- @
--
-- /Since: 3/
newtype FromStringShow1 f a = FromStringShow1
  { fromStringShow1 :: FromStringShow (f (FromTextShow a))
  }
  deriving ( Generic
#if defined(__LANGUAGE_DERIVE_GENERIC1__)
           , Generic1
#endif
#if __GLASGOW_HASKELL__ >= 800
           , Foldable
           , Functor
           , Traversable
#endif
           )

deriving instance Eq      (f (FromTextShow a)) => Eq    (FromStringShow1 f a)
deriving instance Ord     (f (FromTextShow a)) => Ord   (FromStringShow1 f a)
deriving instance Lift    (f (FromTextShow a)) => Lift  (FromStringShow1 f a)
deriving instance ( Typeable f, Typeable a
                  , Data  (f (FromTextShow a))
                  ) => Data  (FromStringShow1 f a)

-- TODO: Manually implement this when you can derive Show1 (someday)
#if __GLASGOW_HASKELL__ < 800
deriving instance Functor     f => Functor     (FromStringShow1 f)
deriving instance Foldable    f => Foldable    (FromStringShow1 f)
deriving instance Traversable f => Traversable (FromStringShow1 f)
deriving instance Typeable FromStringShow1
deriving instance ( Data (f a), Typeable f, Typeable a
                  ) => Data (FromStringShow1 f (a :: *))
#endif

mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f (a, b) = (f a, b)

mapReadS :: (a -> b) -> ReadS a -> ReadS b
mapReadS = fmap . fmap . mapFst

instance (Read (f a), Functor f) => Read (FromStringShow1 f a) where
    readPrec     = coerce $ (fmap . fmap)            FromTextShow (readPrec     :: ReadPrec (f a))
    readsPrec    = coerce $ (fmap . mapReadS . fmap) FromTextShow (readsPrec    :: Int -> ReadS (f a))
    readList     = coerce $ (mapReadS . fmap . fmap) FromTextShow (readList     :: ReadS [f a])
    readListPrec = coerce $ (fmap . fmap . fmap)     FromTextShow (readListPrec :: ReadPrec [f a])

#if defined(NEW_FUNCTOR_CLASSES)

-- | Not available if using @transformers-0.4@
instance (Show1 f, TextShow a) => TextShow (FromStringShow1 f a) where
    showbPrec = liftShowbPrec showbPrec showbList
    showbList = liftShowbList showbPrec showbList

-- | Not available if using @transformers-0.4@
instance Show1 f => TextShow1 (FromStringShow1 f) where
    liftShowbPrec sp sl p =
        showsPrecToShowbPrec'
            (liftShowsPrec
                (showbPrecToShowsPrec' sp)
                (showbToShows'         sl . (coerce :: [FromTextShow a] -> FromTextShow [a])))
            p . fromStringShow1
    liftShowbList sp sl =
        showsToShowb'
            (liftShowList
                (showbPrecToShowsPrec' sp)
                (showbToShows'         sl . (coerce :: [FromTextShow a] -> FromTextShow [a])))
            . coerceList
      where
        coerceList :: [FromStringShow1 f a] -> FromStringShow [f (FromTextShow a)]
        coerceList = coerce
#endif

contraShowS :: (b -> a) -> (a -> ShowS) -> (b -> ShowS)
contraShowS f s = s . f

contraShowsPrec :: (b -> a) -> ShowStringPrec a -> ShowStringPrec b
contraShowsPrec = fmap . contraShowS

instance (Show (f a), Functor f) => Show (FromStringShow1 f a) where
    showsPrec = coerce $ (contraShowsPrec . fmap)    fromTextShow (showsPrec :: ShowStringPrec (f a))
    showList  = coerce $ (contraShowS . fmap . fmap) fromTextShow (showList  :: [f a] -> ShowS)

-------------------------------------------------------------------------------

-- | An adapter newtype, suitable for @DerivingVia@.
-- The @String@ 'Show1' instance for 'FromTextShow1' is based on its
-- 'TextShow1' instance. That is,
--
-- @
-- 'liftShowsPrec' sp sl p ('FromTextShow1' x) =
--     'showbPrecToShowsPrec' ('liftShowbPrec' ('showsPrecToShowbPrec' sp)
--                                             ('showsToShowb'         sl))
--                            p x
-- @
--
-- /Since: 3/
newtype FromTextShow1 f a = FromTextShow1
  { fromTextShow1 :: FromTextShow (f (FromStringShow a))
  }
  deriving ( Generic
#if defined(__LANGUAGE_DERIVE_GENERIC1__)
           , Generic1
#endif
#if __GLASGOW_HASKELL__ >= 800
           , Foldable
           , Functor
           , Traversable
#endif
           )

deriving instance Eq      (f (FromStringShow a)) => Eq    (FromTextShow1 f a)
deriving instance Ord     (f (FromStringShow a)) => Ord   (FromTextShow1 f a)
deriving instance Lift    (f (FromStringShow a)) => Lift  (FromTextShow1 f a)
deriving instance ( Typeable f, Typeable a
                  , Data  (f (FromStringShow a))
                  ) => Data  (FromTextShow1 f a)

#if __GLASGOW_HASKELL__ < 800
deriving instance Functor     f => Functor     (FromTextShow1 f)
deriving instance Foldable    f => Foldable    (FromTextShow1 f)
deriving instance Traversable f => Traversable (FromTextShow1 f)
deriving instance Typeable FromTextShow1
deriving instance ( Data (f a), Typeable f, Typeable a
                  ) => Data (FromTextShow1 f (a :: *))
#endif

instance (Read (f a), Functor f) => Read (FromTextShow1 f a) where
    readPrec     = coerce $ (fmap . fmap)            FromStringShow (readPrec     :: ReadPrec (f a))
    readsPrec    = coerce $ (fmap . mapReadS . fmap) FromStringShow (readsPrec    :: Int -> ReadS (f a))
    readList     = coerce $ (mapReadS . fmap . fmap) FromStringShow (readList     :: ReadS [f a])
    readListPrec = coerce $ (fmap . fmap . fmap)     FromStringShow (readListPrec :: ReadPrec [f a])

#if defined(NEW_FUNCTOR_CLASSES)
-- | Not available if using @transformers-0.4@
instance (TextShow1 f, Show a) => Show (FromTextShow1 f a) where
    showsPrec = liftShowsPrec showsPrec showList
    showList  = liftShowList showsPrec showList

ourLiftShowsPrec
    :: TextShow1 f
    => (Int -> a -> ShowS) -> ([a] -> ShowS) -> Int -> FromTextShow1 f a -> ShowS
ourLiftShowsPrec sp sl p
      = showbPrecToShowsPrec'
          (liftShowbPrec
              (showsPrecToShowbPrec' sp)
              (showsToShowb'         sl . (coerce :: [FromStringShow a] -> FromStringShow [a])))
          p . fromTextShow1

ourLiftShowList
    :: TextShow1 f
    => (Int -> a -> ShowS) -> ([a] -> ShowS) -> [FromTextShow1 f a] -> ShowS
ourLiftShowList sp sl =
        showbToShows'
            (liftShowbList
                (showsPrecToShowbPrec' sp)
                (showsToShowb'         sl . (coerce :: [FromStringShow a] -> FromStringShow [a])))
            . coerceList
      where
        coerceList :: [FromTextShow1 f a] -> FromTextShow [f (FromStringShow a)]
        coerceList = coerce

#endif

instance (TextShow1 f) => Show1 (FromTextShow1 f) where
#if defined(NEW_FUNCTOR_CLASSES)
    liftShowList = ourLiftShowList
    liftShowsPrec = ourLiftShowsPrec
#else
    showsPrec1 p
      = showbPrecToShowsPrec (liftShowbPrec (showsPrecToShowbPrec sp)
                                            (showsToShowb         sl))
                             p . fromTextShow1
#endif

contraBuilder :: (b -> a) -> (a -> Builder) -> (b -> Builder)
contraBuilder f s = s . f

contraShowbPrec :: (b -> a) -> ShowBuilderPrec a -> ShowBuilderPrec b
contraShowbPrec = fmap . contraBuilder

instance (TextShow (f a), Functor f) => TextShow (FromTextShow1 f a) where
    showbPrec = coerce $ (contraShowbPrec . fmap)      fromStringShow (showbPrec :: ShowBuilderPrec (f a))
    showbList = coerce $ (contraBuilder . fmap . fmap) fromStringShow (showbList :: [f a] -> Builder)

-------------------------------------------------------------------------------

-- | An adapter newtype, suitable for @DerivingVia@.
-- The 'TextShow2' instance for 'FromStringShow2' is based on its @String@
-- 'Show2' instance. That is,
--
-- @
-- 'liftShowbPrec2' sp1 sl1 sp2 sl2 p ('FromStringShow2' x) =
--     'showsPrecToShowbPrec' ('liftShowsPrec2' ('showbPrecToShowsPrec' sp1)
--                                              ('showbToShows'         sl1)
--                                              ('showbPrecToShowsPrec' sp2)
--                                              ('showbToShows'         sl2))
--                            p x
-- @
--
-- /Since: 3/
newtype FromStringShow2 f a b = FromStringShow2 { fromStringShow2 :: f a b }
  deriving ( Eq
           , Ord
           , Generic
#if defined(__LANGUAGE_DERIVE_GENERIC1__)
           , Generic1
#endif
#if __GLASGOW_HASKELL__ >= 800
           , Data
           , Foldable
           , Functor
           , Lift
           , Traversable
#endif
           )

#if __GLASGOW_HASKELL__ < 800
deriving instance Functor     (f a) => Functor     (FromStringShow2 f a)
deriving instance Foldable    (f a) => Foldable    (FromStringShow2 f a)
deriving instance Traversable (f a) => Traversable (FromStringShow2 f a)
deriving instance Typeable FromStringShow2
deriving instance ( Data (f a b), Typeable f, Typeable a, Typeable b
                  ) => Data (FromStringShow2 f (a :: *) (b :: *))
#endif

instance Read (f a b) => Read (FromStringShow2 f a b) where
    readPrec     = coerce (readPrec     :: ReadPrec (f a b))
    readsPrec    = coerce (readsPrec    :: Int -> ReadS (f a b))
    readList     = coerce (readList     :: ReadS [f a b])
    readListPrec = coerce (readListPrec :: ReadPrec [f a b])

#if defined(NEW_FUNCTOR_CLASSES)
-- TODO: Manually implement this when you can derive Show2 (someday)
-- | Not available if using @transformers-0.4@
deriving instance Show2 f => Show2 (FromStringShow2 f)

-- | Not available if using @transformers-0.4@
instance (Show2 f, Show a, Show b) => TextShow (FromStringShow2 f a b) where
    showbPrec = liftShowbPrec (showsPrecToShowbPrec showsPrec)
                              (showsToShowb showList)
    showbList = liftShowbList (showsPrecToShowbPrec showsPrec)
                              (showsToShowb showList)

-- | Not available if using @transformers-0.4@
instance (Show2 f, Show a) => TextShow1 (FromStringShow2 f a) where
    liftShowbPrec = liftShowbPrec2 (showsPrecToShowbPrec showsPrec)
                                   (showsToShowb showList)
    liftShowbList = liftShowbList2 (showsPrecToShowbPrec showsPrec)
                                   (showsToShowb showList)

-- | Not available if using @transformers-0.4@
instance Show2 f => TextShow2 (FromStringShow2 f) where
    liftShowbPrec2 sp1 sl1 sp2 sl2 p =
        showsPrecToShowbPrec (liftShowsPrec2 (showbPrecToShowsPrec sp1)
                                             (showbToShows         sl1)
                                             (showbPrecToShowsPrec sp2)
                                             (showbToShows         sl2))
                             p . fromStringShow2
    liftShowbList2 sp1 sl1 sp2 sl2 =
        showsToShowb (liftShowList2 (showbPrecToShowsPrec sp1)
                                    (showbToShows         sl1)
                                    (showbPrecToShowsPrec sp2)
                                    (showbToShows         sl2))
                     . coerceList
      where
        coerceList :: [FromStringShow2 f a b] -> [f a b]
        coerceList = coerce

-- | Not available if using @transformers-0.4@
instance (Show2 f, Show a, Show b) => Show (FromStringShow2 f a b) where
    showsPrec = showsPrec2
    showList  = liftShowList2 showsPrec showList showsPrec showList

-- | Not available if using @transformers-0.4@
instance (Show2 f, Show a) => Show1 (FromStringShow2 f a) where
    liftShowsPrec = liftShowsPrec2 showsPrec showList
    liftShowList  = liftShowList2  showsPrec showList
#endif

-------------------------------------------------------------------------------

-- | An adapter newtype, suitable for @DerivingVia@.
-- The @String@ 'Show2' instance for 'FromTextShow2' is based on its
-- 'TextShow2' instance. That is,
--
-- @
-- liftShowsPrec2 sp1 sl1 sp2 sl2 p ('FromTextShow2' x) =
--     'showbPrecToShowsPrec' ('liftShowbPrec2' ('showsPrecToShowbPrec' sp1)
--                                              ('showsToShowb'         sl1)
--                                              ('showsPrecToShowbPrec' sp2)
--                                              ('showsToShowb'         sl2))
--                            p x
-- @
--
-- /Since: 3/
newtype FromTextShow2 f a b = FromTextShow2 { fromTextShow2 :: f a b }
  deriving ( Eq
           , Ord
           , Generic
#if defined(__LANGUAGE_DERIVE_GENERIC1__)
           , Generic1
#endif
#if __GLASGOW_HASKELL__ >= 800
           , Data
           , Foldable
           , Functor
           , Lift
           , TextShow2
           , Traversable
#endif
           )

#if __GLASGOW_HASKELL__ < 800
deriving instance TextShow2    f    => TextShow2   (FromTextShow2 f)
deriving instance Functor     (f a) => Functor     (FromTextShow2 f a)
deriving instance Foldable    (f a) => Foldable    (FromTextShow2 f a)
deriving instance Traversable (f a) => Traversable (FromTextShow2 f a)
deriving instance Typeable FromTextShow2
deriving instance ( Data (f a b), Typeable f, Typeable a, Typeable b
                  ) => Data (FromTextShow2 f (a :: *) (b :: *))
#endif

instance Read (f a b) => Read (FromTextShow2 f a b) where
    readPrec     = coerce (readPrec     :: ReadPrec (f a b))
    readsPrec    = coerce (readsPrec    :: Int -> ReadS (f a b))
    readList     = coerce (readList     :: ReadS [f a b])
    readListPrec = coerce (readListPrec :: ReadPrec [f a b])

#if defined(NEW_FUNCTOR_CLASSES)
-- | Not available if using @transformers-0.4@
instance (TextShow2 f, TextShow a, TextShow b) => Show (FromTextShow2 f a b) where
    showsPrec = ourLiftShowsPrec1 (showbPrecToShowsPrec showbPrec)
                              (showbToShows showbList)
    showList  = ourLiftShowList1  (showbPrecToShowsPrec showbPrec)
                              (showbToShows showbList)

ourLiftShowsPrec1
    :: (TextShow2 f, TextShow a) => (Int -> b -> ShowS) -> ([b] -> ShowS) -> Int -> FromTextShow2 f a b -> ShowS
ourLiftShowsPrec1 = ourLiftShowsPrec2 (showbPrecToShowsPrec showbPrec)
                               (showbToShows         showbList)

ourLiftShowList1
    :: (TextShow2 f, TextShow a) => (Int -> b -> ShowS) -> ([b] -> ShowS) -> [FromTextShow2 f a b] -> ShowS
ourLiftShowList1 = ourLiftShowList2 (showbPrecToShowsPrec showbPrec)
                             (showbToShows         showbList)

-- | Not available if using @transformers-0.4@
instance (TextShow2 f, TextShow a, forall b. Show b => TextShow b) => Show1 (FromTextShow2 f a) where
    liftShowsPrec = ourLiftShowsPrec1
    liftShowList = ourLiftShowList1

ourLiftShowsPrec2
    :: TextShow2 f => (Int -> a -> ShowS) -> ([a] -> ShowS) -> (Int -> b -> ShowS) -> ([b] -> ShowS) -> Int -> FromTextShow2 f a b -> ShowS
ourLiftShowsPrec2 sp1 sl1 sp2 sl2 p =
    showbPrecToShowsPrec (liftShowbPrec2 (showsPrecToShowbPrec sp1)
                                         (showsToShowb         sl1)
                                         (showsPrecToShowbPrec sp2)
                                         (showsToShowb         sl2))
                         p . fromTextShow2

ourLiftShowList2
    :: TextShow2 f => (Int -> a -> ShowS) -> ([a] -> ShowS) -> (Int -> b -> ShowS) -> ([b] -> ShowS) -> [FromTextShow2 f a b] -> ShowS
ourLiftShowList2 sp1 sl1 sp2 sl2 =
    showbToShows (liftShowbList2 (showsPrecToShowbPrec sp1)
                                 (showsToShowb         sl1)
                                 (showsPrecToShowbPrec sp2)
                                 (showsToShowb         sl2))
                 . coerceList
  where
    coerceList :: [FromTextShow2 f a b] -> [f a b]
    coerceList = coerce

-- | Not available if using @transformers-0.4@
instance (TextShow2 f) => Show2 (FromTextShow2 f) where
    liftShowsPrec2 = ourLiftShowsPrec2
    liftShowList2 = ourLiftShowList2
#endif

instance (TextShow2 f, TextShow a, TextShow b) => TextShow (FromTextShow2 f a b) where
    showbPrec = showbPrec2
    showbList = liftShowbList2 showbPrec showbList showbPrec showbList

instance (TextShow2 f, TextShow a) => TextShow1 (FromTextShow2 f a) where
    liftShowbPrec = liftShowbPrec2 showbPrec showbList
    liftShowbList = liftShowbList2 showbPrec showbList

-------------------------------------------------------------------------------

#if !defined(NEW_FUNCTOR_CLASSES)
liftShowsPrec :: (Show1 f, Show a) => (Int -> a -> ShowS) -> ([a] -> ShowS)
              -> Int -> f a -> ShowS
liftShowsPrec _ _ = showsPrec1

liftShowList :: (Show1 f, Show a) => (Int -> a -> ShowS) -> ([a] -> ShowS)
              -> [f a] -> ShowS
liftShowList sp' sl' = showListWith (liftShowsPrec sp' sl' 0)

sp :: Int -> a -> ShowS
sp  = undefined

sl :: [a] -> ShowS
sl  = undefined
#endif

-------------------------------------------------------------------------------

$(deriveBifunctor     ''FromStringShow2)
$(deriveBifunctor     ''FromTextShow2)
$(deriveBifoldable    ''FromStringShow2)
$(deriveBifoldable    ''FromTextShow2)
$(deriveBitraversable ''FromStringShow2)
$(deriveBitraversable ''FromTextShow2)

#if __GLASGOW_HASKELL__ < 800
$(deriveLift ''FromStringShow)
$(deriveLift ''FromTextShow)

instance Lift (f a) => Lift (FromStringShow1 f a) where
    lift = $(makeLift ''FromStringShow1)
instance Lift (f a) => Lift (FromTextShow1 f a) where
    lift = $(makeLift ''FromTextShow1)

instance Lift (f a b) => Lift (FromStringShow2 f a b) where
    lift = $(makeLift ''FromStringShow2)
instance Lift (f a b) => Lift (FromTextShow2 f a b) where
    lift = $(makeLift ''FromTextShow2)
#endif

#if !defined(__LANGUAGE_DERIVE_GENERIC1__)
$(Generics.deriveMeta           ''FromStringShow1)
$(Generics.deriveRepresentable1 ''FromStringShow1)
$(Generics.deriveMeta           ''FromTextShow1)
$(Generics.deriveRepresentable1 ''FromTextShow1)
$(Generics.deriveMeta           ''FromStringShow2)
$(Generics.deriveRepresentable1 ''FromStringShow2)
$(Generics.deriveMeta           ''FromTextShow2)
$(Generics.deriveRepresentable1 ''FromTextShow2)
#endif
