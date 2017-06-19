{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE DataKinds          #-}
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
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'Options' and related datatypes.

/Since: 3.4/
-}
module TextShow.Options (Options(..), GenTextMethods(..), defaultOptions) where

import           Data.Data (Data, Typeable)
import           Data.Ix (Ix)

import           GHC.Generics (Generic)

import           Language.Haskell.TH.Lift

-- | Options that specify how to derive 'TextShow' instances using Template Haskell.
--
-- /Since: 3.4/
newtype Options = Options
  { genTextMethods :: GenTextMethods
    -- ^ When Template Haskell should generate definitions for methods which
    --   return @Text@?
  } deriving ( Data
             , Eq
             , Generic
             , Ord
             , Read
             , Show
             , Typeable
#if __GLASGOW_HASKELL__ >= 800
             , Lift
#endif
             )

-- | When should Template Haskell generate implementations for the methods of
-- 'TextShow' which return @Text@?
--
-- /Since: 3.4/
data GenTextMethods
  = AlwaysTextMethods    -- ^ Always generate them.
  | SometimesTextMethods -- ^ Only generate when @text-show@ feels it's appropriate.
  | NeverTextMethods     -- ^ Never generate them under any circumstances.
  deriving ( Bounded
           , Data
           , Enum
           , Eq
           , Generic
           , Ix
           , Ord
           , Read
           , Show
           , Typeable
#if __GLASGOW_HASKELL__ >= 800
           , Lift
#endif
           )

-- | Sensible default 'Options'.
--
-- /Since: 3.4/
defaultOptions :: Options
defaultOptions = Options { genTextMethods = SometimesTextMethods }

-------------------------------------------------------------------------------

#if __GLASGOW_HASKELL__ < 800
$(deriveLift ''Options)
$(deriveLift ''GenTextMethods)
#endif
