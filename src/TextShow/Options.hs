{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveLift         #-}

{-|
Module:      TextShow.Options
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'Options' and related datatypes.

/Since: 3.4/
-}
module TextShow.Options (Options(..), GenTextMethods(..), defaultOptions) where

import Data.Data (Data)
import Data.Ix (Ix)

import GHC.Generics (Generic)

import Language.Haskell.TH.Syntax (Lift)

-- | Options that specify how to derive 'TextShow' instances using Template Haskell.
--
-- /Since: 3.4/
data Options = Options
  { genTextMethods :: GenTextMethods
    -- ^ When Template Haskell should generate definitions for methods which
    --   return @Text@?
    --
    --   /Since: 3.4/
  , emptyCaseBehavior :: Bool
    -- ^ If 'True', derived instances for empty data types (i.e., ones with
    --   no data constructors) will use the @EmptyCase@ language extension.
    --   If 'False', derived instances will simply use 'seq' instead.
    --
    --   /Since: 3.7/
  } deriving ( Data
             , Eq
             , Generic
             , Ord
             , Read
             , Show
             , Lift
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
           , Lift
           )

-- | Sensible default 'Options'.
--
-- /Since: 3.4/
defaultOptions :: Options
defaultOptions =
  Options { genTextMethods    = SometimesTextMethods
          , emptyCaseBehavior = False
          }
