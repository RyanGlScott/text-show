{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE DeriveGeneric              #-}
#else
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
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

#if __GLASGOW_HASKELL__ >= 702
import           GHC.Generics (Generic)
# if __GLASGOW_HASKELL__ >= 706
import           GHC.Generics (Generic1)
# endif
#else
import qualified Generics.Deriving.TH as Generics (deriveAll)
#endif

import           Prelude ()
import           Prelude.Compat

import           Text.Read (Read(..), readListPrecDefault)

import           TextShow.Classes (TextShow(..), TextShow1(..),
                                   showbToShows, showsToShowb)

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
    readPrec = FromStringShow <$> readPrec
    INLINE_INST_FUN(readPrec)

    readListPrec = readListPrecDefault
    INLINE_INST_FUN(readListPrec)

instance Show a => TextShow (FromStringShow a) where
    showbPrec p = showsToShowb showsPrec p . fromStringShow
    INLINE_INST_FUN(showbPrec)

instance Show a => Show (FromStringShow a) where
    showsPrec p = showsPrec p . fromStringShow
    INLINE_INST_FUN(showsPrec)

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
    readPrec = FromTextShow <$> readPrec
    INLINE_INST_FUN(readPrec)

    readListPrec = readListPrecDefault
    INLINE_INST_FUN(readListPrec)

instance TextShow a => Show (FromTextShow a) where
    showsPrec p = showbToShows showbPrec p . fromTextShow
    INLINE_INST_FUN(showsPrec)

instance TextShow1 FromTextShow where
    showbPrecWith sp p = sp p . fromTextShow
    INLINE_INST_FUN(showbPrecWith)

-------------------------------------------------------------------------------

#if __GLASGOW_HASKELL__ < 702
$(Generics.deriveAll ''FromStringShow)
$(Generics.deriveAll ''FromTextShow)
#endif
