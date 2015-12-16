{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE DeriveGeneric              #-}
#endif

#if __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE AutoDeriveTypeable         #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE PolyKinds                  #-}
#endif

{-|
Module:      TextShow.FromStringTextShow
Copyright:   (C) 2014-2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

The 'FromStringShow' and 'FromTextShow' data types.
-}
module TextShow.FromStringTextShow (FromStringShow(..), FromTextShow(..)) where

import           Data.Data (Data, Typeable)

#if __GLASGOW_HASKELL__ < 706
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

import           Text.ParserCombinators.ReadPrec (ReadPrec)
import           Text.Read (Read(..))

import           TextShow.Classes (TextShow(..), TextShow1(..),
                                   showbToShows, showsToShowb)
import           TextShow.Utils (coerce)

#include "inline.h"

-- | The 'TextShow' instance for 'FromStringShow' is based on its @String@
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
#if __GLASGOW_HASKELL__ >= 702
           , Generic
# if __GLASGOW_HASKELL__ >= 706
           , Generic1
# endif
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
    showbPrec p = showsToShowb showsPrec p . fromStringShow
    INLINE_INST_FUN(showbPrec)

    showb = showsToShowb (const shows) 0 . fromStringShow
    INLINE_INST_FUN(showb)

    showbList l = showsToShowb (const showList) 0 (coerce l :: [a])
    INLINE_INST_FUN(showbList)

instance Show a => Show (FromStringShow a) where
    showsPrec = coerce (showsPrec :: Int -> a -> ShowS)
    show      = coerce (show      :: a -> String)
    showList  = coerce (showList  :: [a] -> ShowS)

instance TextShow1 FromStringShow where
    showbPrecWith sp p =
        showsToShowb (showbToShows sp) p . fromStringShow
    INLINE_INST_FUN(showbPrecWith)

-- | The @String@ 'Show' instance for 'FromTextShow' is based on its
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
#if __GLASGOW_HASKELL__ >= 702
           , Generic
# if __GLASGOW_HASKELL__ >= 706
           , Generic1
# endif
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
    showsPrec p = showbToShows showbPrec p . fromTextShow
    INLINE_INST_FUN(showsPrec)

    show (FromTextShow x) = showbToShows (const showb) 0 x ""
    INLINE_INST_FUN(show)

    showList l = showbToShows (const showbList) 0 (coerce l :: [a])
    INLINE_INST_FUN(showList)

instance TextShow1 FromTextShow where
    showbPrecWith sp p = sp p . fromTextShow
    INLINE_INST_FUN(showbPrecWith)

-------------------------------------------------------------------------------

#if __GLASGOW_HASKELL__ < 706
$(Generics.deriveMeta           ''FromStringShow)
$(Generics.deriveRepresentable1 ''FromStringShow)
$(Generics.deriveMeta           ''FromTextShow)
$(Generics.deriveRepresentable1 ''FromTextShow)
#endif

#if __GLASGOW_HASKELL__ < 702
$(Generics.deriveRepresentable0 ''FromStringShow)
$(Generics.deriveRepresentable0 ''FromTextShow)
#endif
