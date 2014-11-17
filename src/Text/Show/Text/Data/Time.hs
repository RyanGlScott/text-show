{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Show.Text.Foreign.Data.Time
-- Copyright   :  (C) 2014 Ryan Scott
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  Experimental
-- Portability :  GHC
-- 
-- Monomorphic 'Show' functions for data types in the @time@ library. These
-- are included for convenience (and because @time@ is a dependency of this
-- library).
----------------------------------------------------------------------------
module Text.Show.Text.Data.Time (
      showbDay
    , showbDiffTime
    , showbUTCTime
    , showbNominalDiffTime
    , showbAbsoluteTime
    , showbTimeZone
    , showbTimeOfDay
    , showbLocalTime
    ) where

import Data.Monoid ((<>))
import Data.Text.Buildable (build)
import Data.Text.Lazy.Builder (Builder)
import Data.Time.Calendar (Day)
import Data.Time.Clock (DiffTime, UTCTime, NominalDiffTime)
import Data.Time.Clock.TAI (AbsoluteTime, taiToUTCTime)
import Data.Time.LocalTime (TimeZone, TimeOfDay, LocalTime,
                            utc, utcToLocalTime)

import Prelude hiding (Show)

import Text.Show.Text.Class (Show(showb))

-- | Convert a 'Day' into a 'Builder'.
showbDay :: Day -> Builder
showbDay = build
{-# INLINE showbDay #-}

-- | Convert a 'DiffTime' into a 'Builder'.
showbDiffTime :: DiffTime -> Builder
showbDiffTime = build
{-# INLINE showbDiffTime #-}

-- | Convert a 'UTCTime' into a 'Builder'.
showbUTCTime :: UTCTime -> Builder
showbUTCTime = build
{-# INLINE showbUTCTime #-}

-- | Convert a 'NominalDiffTime' into a 'Builder'.
showbNominalDiffTime :: NominalDiffTime -> Builder
showbNominalDiffTime = build
{-# INLINE showbNominalDiffTime #-}

-- | Convert a 'AbsoluteTime' into a 'Builder'.
showbAbsoluteTime :: AbsoluteTime -> Builder
showbAbsoluteTime t = showbLocalTime (utcToLocalTime utc $ taiToUTCTime (const 0) t)
                      <> " TAI" -- ugly, but standard apparently
{-# INLINE showbAbsoluteTime #-}

-- | Convert a 'TimeZone' into a 'Builder'.
showbTimeZone :: TimeZone -> Builder
showbTimeZone = build
{-# INLINE showbTimeZone #-}

-- | Convert a 'TimeOfDay' into a 'Builder'.
showbTimeOfDay :: TimeOfDay -> Builder
showbTimeOfDay = build
{-# INLINE showbTimeOfDay #-}

-- | Convert a 'LocalTime' into a 'Builder'.
showbLocalTime :: LocalTime -> Builder
showbLocalTime = build
{-# INLINE showbLocalTime #-}

instance Show Day where
    showb = showbDay
    {-# INLINE showb #-}

instance Show DiffTime where
    showb = showbDiffTime
    {-# INLINE showb #-}

instance Show UTCTime where
    showb = showbUTCTime
    {-# INLINE showb #-}

instance Show NominalDiffTime where
    showb = showbNominalDiffTime
    {-# INLINE showb #-}

instance Show AbsoluteTime where
    showb = showbAbsoluteTime
    {-# INLINE showb #-}

instance Show TimeZone where
    showb = showbTimeZone
    {-# INLINE showb #-}

instance Show TimeOfDay where
    showb = showbTimeOfDay
    {-# INLINE showb #-}

instance Show LocalTime where
    showb = showbLocalTime
    {-# INLINE showb #-}