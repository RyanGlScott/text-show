{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE PolyKinds                  #-}
#endif

#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE DeriveLift                 #-}
#endif

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

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
import           Data.Data (Data, Typeable)
import           Data.Functor.Classes (Show1(..))

#if !defined(__LANGUAGE_DERIVE_GENERIC1__)
import qualified Generics.Deriving.TH as Generics
#endif

#if __GLASGOW_HASKELL__ >= 706
import           GHC.Generics (Generic, Generic1)
#endif

import           Language.Haskell.TH.Lift

import           Prelude ()
import           Prelude.Compat

import           Text.ParserCombinators.ReadPrec (ReadPrec)
import           Text.Read (Read(..))

import           TextShow.Classes (TextShow(..), TextShow1(..), TextShow2(..),
                                   showbPrec1, showbPrec2,
                                   showbPrecToShowsPrec, showsPrecToShowbPrec,
                                   showbToShows, showsToShowb)
import           TextShow.Utils (coerce)

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
#if __GLASGOW_HASKELL__ >= 706
           , Generic
           , Generic1
#endif
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

instance TextShow1 FromStringShow where
    liftShowbPrec sp' _ p =
        showsPrecToShowbPrec (showbPrecToShowsPrec sp') p . fromStringShow

    liftShowbList _ sl' = showsToShowb (showbToShows sl') . coerceList
      where
        coerceList :: [FromStringShow a] -> [a]
        coerceList = coerce

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
#if __GLASGOW_HASKELL__ >= 706
           , Generic
           , Generic1
#endif
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

instance TextShow a => Show (FromTextShow a) where
    showsPrec p = showbPrecToShowsPrec showbPrec p . fromTextShow
    show (FromTextShow x) = showbToShows showb x ""
    showList l = showbToShows showbList (coerce l :: [a])

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
newtype FromStringShow1 f a = FromStringShow1 { fromStringShow1 :: f a }
  deriving ( Eq
           , Ord
#if __GLASGOW_HASKELL__ >= 706
           , Generic
# if defined(__LANGUAGE_DERIVE_GENERIC1__)
           , Generic1
# endif
#endif
#if __GLASGOW_HASKELL__ >= 800
           , Data
           , Foldable
           , Functor
           , Lift
           , Show1 -- TODO: Manually implement this when you
                   -- can derive Show1 (someday)
           , Traversable
#endif
           )

#if __GLASGOW_HASKELL__ < 800
-- TODO: Manually implement this when you can derive Show1 (someday)
deriving instance Show1       f => Show1       (FromStringShow1 f)
deriving instance Functor     f => Functor     (FromStringShow1 f)
deriving instance Foldable    f => Foldable    (FromStringShow1 f)
deriving instance Traversable f => Traversable (FromStringShow1 f)

# if __GLASGOW_HASKELL__ >= 708
deriving instance Typeable FromStringShow1
deriving instance ( Data (f a), Typeable f, Typeable a
                  ) => Data (FromStringShow1 f (a :: *))
# endif
#endif

instance Read (f a) => Read (FromStringShow1 f a) where
    readPrec     = coerce (readPrec     :: ReadPrec (f a))
    readsPrec    = coerce (readsPrec    :: Int -> ReadS (f a))
    readList     = coerce (readList     :: ReadS [f a])
    readListPrec = coerce (readListPrec :: ReadPrec [f a])

#if defined(NEW_FUNCTOR_CLASSES)
-- | Not available if using @transformers-0.4@
instance (Show1 f, Show a) => TextShow (FromStringShow1 f a) where
    showbPrec = liftShowbPrec (showsPrecToShowbPrec showsPrec)
                              (showsToShowb showList)
    showbList = liftShowbList (showsPrecToShowbPrec showsPrec)
                              (showsToShowb showList)

-- | Not available if using @transformers-0.4@
instance Show1 f => TextShow1 (FromStringShow1 f) where
    liftShowbPrec sp sl p =
        showsPrecToShowbPrec (liftShowsPrec (showbPrecToShowsPrec sp)
                                            (showbToShows         sl))
                             p . fromStringShow1
    liftShowbList sp sl =
        showsToShowb (liftShowList (showbPrecToShowsPrec sp)
                                   (showbToShows         sl))
                     . coerceList
      where
        coerceList :: [FromStringShow1 f a] -> [f a]
        coerceList = coerce
#endif

instance (Show1 f, Show a) => Show (FromStringShow1 f a) where
    showsPrec = showsPrec1
    showList  = liftShowList showsPrec showList

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
newtype FromTextShow1 f a = FromTextShow1 { fromTextShow1 :: f a }
  deriving ( Eq
           , Ord
#if __GLASGOW_HASKELL__ >= 706
           , Generic
# if defined(__LANGUAGE_DERIVE_GENERIC1__)
           , Generic1
# endif
#endif
#if __GLASGOW_HASKELL__ >= 800
           , Data
           , Foldable
           , Functor
           , Lift
           , TextShow1
           , Traversable
#endif
           )

#if __GLASGOW_HASKELL__ < 800
deriving instance TextShow1   f => TextShow1   (FromTextShow1 f)
deriving instance Functor     f => Functor     (FromTextShow1 f)
deriving instance Foldable    f => Foldable    (FromTextShow1 f)
deriving instance Traversable f => Traversable (FromTextShow1 f)

# if __GLASGOW_HASKELL__ >= 708
deriving instance Typeable FromTextShow1
deriving instance ( Data (f a), Typeable f, Typeable a
                  ) => Data (FromTextShow1 f (a :: *))
# endif
#endif

instance Read (f a) => Read (FromTextShow1 f a) where
    readPrec     = coerce (readPrec     :: ReadPrec (f a))
    readsPrec    = coerce (readsPrec    :: Int -> ReadS (f a))
    readList     = coerce (readList     :: ReadS [f a])
    readListPrec = coerce (readListPrec :: ReadPrec [f a])

#if defined(NEW_FUNCTOR_CLASSES)
-- | Not available if using @transformers-0.4@
instance (TextShow1 f, TextShow a) => Show (FromTextShow1 f a) where
    showsPrec = liftShowsPrec (showbPrecToShowsPrec showbPrec)
                              (showbToShows showbList)
    showList  = liftShowList  (showbPrecToShowsPrec showbPrec)
                              (showbToShows showbList)
#endif

instance TextShow1 f => Show1 (FromTextShow1 f) where
#if defined(NEW_FUNCTOR_CLASSES)
    liftShowList sp sl =
        showbToShows (liftShowbList (showsPrecToShowbPrec sp)
                                    (showsToShowb         sl))
                     . coerceList
      where
        coerceList :: [FromTextShow1 f a] -> [f a]
        coerceList = coerce
    liftShowsPrec sp sl p
#else
    showsPrec1 p
#endif
      = showbPrecToShowsPrec (liftShowbPrec (showsPrecToShowbPrec sp)
                                            (showsToShowb         sl))
                             p . fromTextShow1

instance (TextShow1 f, TextShow a) => TextShow (FromTextShow1 f a) where
    showbPrec = showbPrec1
    showbList = liftShowbList showbPrec showbList

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
#if __GLASGOW_HASKELL__ >= 706
           , Generic
# if defined(__LANGUAGE_DERIVE_GENERIC1__)
           , Generic1
# endif
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

# if __GLASGOW_HASKELL__ >= 708
deriving instance Typeable FromStringShow2
deriving instance ( Data (f a b), Typeable f, Typeable a, Typeable b
                  ) => Data (FromStringShow2 f (a :: *) (b :: *))
# endif
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
#if __GLASGOW_HASKELL__ >= 706
           , Generic
# if defined(__LANGUAGE_DERIVE_GENERIC1__)
           , Generic1
# endif
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

# if __GLASGOW_HASKELL__ >= 708
deriving instance Typeable FromTextShow2
deriving instance ( Data (f a b), Typeable f, Typeable a, Typeable b
                  ) => Data (FromTextShow2 f (a :: *) (b :: *))
# endif
#endif

instance Read (f a b) => Read (FromTextShow2 f a b) where
    readPrec     = coerce (readPrec     :: ReadPrec (f a b))
    readsPrec    = coerce (readsPrec    :: Int -> ReadS (f a b))
    readList     = coerce (readList     :: ReadS [f a b])
    readListPrec = coerce (readListPrec :: ReadPrec [f a b])

#if defined(NEW_FUNCTOR_CLASSES)
-- | Not available if using @transformers-0.4@
instance (TextShow2 f, TextShow a, TextShow b) => Show (FromTextShow2 f a b) where
    showsPrec = liftShowsPrec (showbPrecToShowsPrec showbPrec)
                              (showbToShows showbList)
    showList  = liftShowList  (showbPrecToShowsPrec showbPrec)
                              (showbToShows showbList)

-- | Not available if using @transformers-0.4@
instance (TextShow2 f, TextShow a) => Show1 (FromTextShow2 f a) where
    liftShowsPrec = liftShowsPrec2 (showbPrecToShowsPrec showbPrec)
                                   (showbToShows         showbList)
    liftShowList = liftShowList2 (showbPrecToShowsPrec showbPrec)
                                 (showbToShows         showbList)

-- | Not available if using @transformers-0.4@
instance TextShow2 f => Show2 (FromTextShow2 f) where
    liftShowsPrec2 sp1 sl1 sp2 sl2 p =
        showbPrecToShowsPrec (liftShowbPrec2 (showsPrecToShowbPrec sp1)
                                             (showsToShowb         sl1)
                                             (showsPrecToShowbPrec sp2)
                                             (showsToShowb         sl2))
                             p . fromTextShow2
    liftShowList2 sp1 sl1 sp2 sl2 =
        showbToShows (liftShowbList2 (showsPrecToShowbPrec sp1)
                                     (showsToShowb         sl1)
                                     (showsPrecToShowbPrec sp2)
                                     (showsToShowb         sl2))
                     . coerceList
      where
        coerceList :: [FromTextShow2 f a b] -> [f a b]
        coerceList = coerce
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

#if __GLASGOW_HASKELL__ < 706
$(Generics.deriveAll0And1       ''FromStringShow)
$(Generics.deriveAll0And1       ''FromTextShow)
$(Generics.deriveRepresentable0 ''FromStringShow1)
$(Generics.deriveRepresentable0 ''FromStringShow2)
$(Generics.deriveRepresentable0 ''FromTextShow1)
$(Generics.deriveRepresentable0 ''FromTextShow2)
#endif
