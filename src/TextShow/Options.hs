{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE DeriveGeneric      #-}
#endif

#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE PolyKinds          #-}
#endif

#if __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE AutoDeriveTypeable #-}
#endif

#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE DeriveLift         #-}
#endif

{-|
Module:      TextShow.FromStringTextShow
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'Options' and related datatypes.
-}
module TextShow.Options (Options(..), GenTextMethods(..), defaultOptions) where

import           Data.Data (Data, Typeable)
import           Data.Ix (Ix)

#if __GLASGOW_HASKELL__ >= 702
import           GHC.Generics (Generic)
#else
import qualified Generics.Deriving.TH as Generics
#endif

import           Language.Haskell.TH.Lift

-- | Options that specify how to derive 'TextShow' instances using Template Haskell.
newtype Options = Options
  { genTextMethods :: GenTextMethods
    -- ^ When Template Haskell should generate definitions for methods which
    --   return @Text@?
  } deriving ( Data
             , Eq
             , Ord
             , Read
             , Show
             , Typeable
#if __GLASGOW_HASKELL__ >= 702
             , Generic
#endif
#if __GLASGOW_HASKELL__ >= 800
             , Lift
#endif
             )

-- | When should Template Haskell generate implementations for the methods of
-- 'TextShow' which return @Text@?
data GenTextMethods
  = AlwaysTextMethods    -- ^ Always generate them.
  | SometimesTextMethods -- ^ Only generate when @text-show@ feels it's appropriate.
  | NeverTextMethods     -- ^ Never generate them under any circumstances.
  deriving ( Bounded
           , Data
           , Enum
           , Eq
           , Ix
           , Ord
           , Read
           , Show
           , Typeable
#if __GLASGOW_HASKELL__ >= 702
           , Generic
#endif
#if __GLASGOW_HASKELL__ >= 800
           , Lift
#endif
           )

-- | Sensible default 'Options'.
defaultOptions :: Options
defaultOptions = Options { genTextMethods = SometimesTextMethods }

-------------------------------------------------------------------------------

#if __GLASGOW_HASKELL__ < 702
$(Generics.deriveAll ''Options)
$(Generics.deriveAll ''GenTextMethods)
#endif

#if __GLASGOW_HASKELL__ < 800
$(deriveLift ''Options)
$(deriveLift ''GenTextMethods)
#endif
