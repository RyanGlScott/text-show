{-# LANGUAGE CPP, DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
#if MIN_VERSION_base(4,4,0)
{-# LANGUAGE DeriveGeneric #-}
#endif
module Text.Show.Text.Newtypes (
      FromStringShow(..)
    , LitChar(..)
    , LitString(..)
    ) where

import           Data.Data (Data, Typeable)
import           Data.Ix (Ix)
import           Data.String (IsString)
import           Data.Text.Lazy.Builder (fromString)

import           Foreign.Storable (Storable)

#if MIN_VERSION_base(4,4,0)
import           GHC.Generics (Generic, Generic1)
#endif

import           Prelude hiding (Show)

import           Text.Printf (IsChar, PrintfArg, PrintfType)
import qualified Text.Show as S (Show(showsPrec))
import qualified Text.Show.Text.Classes as T (Show)
import           Text.Show.Text.Classes (showb, showbPrec)
import           Text.Show.Text.Data.Char (showbLitChar, showbLitString)

#include "inline.h"

newtype FromStringShow a = FromStringShow { fromStringShow :: a }
  deriving ( Bounded
           , Data
           , Enum
           , Eq
           , Floating
           , Fractional
#if MIN_VERSION_base(4,4,0)
           , Generic
           , Generic1
#endif
           , Integral
           , Num
           , Ord
           , Read
           , Real
           , RealFloat
           , RealFrac
           , S.Show
           , Typeable
           )

instance S.Show a => T.Show (FromStringShow a) where
    showbPrec p (FromStringShow x) = fromString $ S.showsPrec p x ""
    INLINE(showbPrec)

newtype LitChar = LitChar { getLitChar :: Char }
  deriving ( Bounded
           , Data
           , Enum
           , Eq
#if MIN_VERSION_base(4,4,0)
           , Generic
#endif
           , IsChar
           , Ix
           , Ord
           , PrintfArg
           , Read
           , S.Show
           , Storable
           , Typeable
           )

instance T.Show LitChar where
    showb = showbLitChar . getLitChar
    INLINE(showb)

newtype LitString = LitString { getLitString :: String }
  deriving ( Data
           , Eq
#if MIN_VERSION_base(4,4,0)
           , Generic
#endif
           , IsString
           , Ord
           , PrintfArg
           , PrintfType
           , Read
           , S.Show
           , Typeable
           )

instance T.Show LitString where
    showb = showbLitString . getLitString
    INLINE(showb)