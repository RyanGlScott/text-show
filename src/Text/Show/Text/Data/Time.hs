{-# LANGUAGE CPP, NoImplicitPrelude, OverloadedStrings #-}
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
    , showbZonedTime
    ) where

import           Data.Text.Lazy.Builder (Builder)
import           Data.Time.Calendar (Day)
import           Data.Time.Clock (DiffTime, UTCTime, NominalDiffTime)
import           Data.Time.Clock.TAI (AbsoluteTime, taiToUTCTime)
import           Data.Time.LocalTime (TimeZone(..), TimeOfDay(..), LocalTime(..),
                                      ZonedTime(..), utc, utcToLocalTime)

import           Prelude hiding (Show)

import           Text.Show.Text.Class (Show(showb))
import           Text.Show.Text.Utils ((<>))

#if defined(TEXT_FORMAT)
import           Data.Text.Buildable (build)
#else
import           Data.Fixed (Pico)
import           Data.Text.Lazy.Builder (fromString)
import           Data.Time.Calendar (toGregorian)
import           Data.Time.Format (NumericPadOption)
import           Data.Time.LocalTime (utcToZonedTime)

import qualified Prelude as P

import           Text.Show.Text.Data.Fixed (showbFixed)
import           Text.Show.Text.Data.Integral ()
import           Text.Show.Text.Utils (lengthB, replicateB, s)
#endif

-- | Convert a 'Day' into a 'Builder'.
showbDay :: Day -> Builder
#if defined(TEXT_FORMAT)
showbDay = build
#else
showbDay = showbGregorian
#endif
{-# INLINE showbDay #-}

-- | Convert a 'DiffTime' into a 'Builder'.
showbDiffTime :: DiffTime -> Builder
#if defined(TEXT_FORMAT)
showbDiffTime = build
#else
showbDiffTime = fromString . P.show
#endif
{-# INLINE showbDiffTime #-}

-- | Convert a 'UTCTime' into a 'Builder'.
showbUTCTime :: UTCTime -> Builder
#if defined(TEXT_FORMAT)
showbUTCTime = build
#else
showbUTCTime = showb . utcToZonedTime utc
#endif
{-# INLINE showbUTCTime #-}

-- | Convert a 'NominalDiffTime' into a 'Builder'.
showbNominalDiffTime :: NominalDiffTime -> Builder
#if defined(TEXT_FORMAT)
showbNominalDiffTime = build
#else
showbNominalDiffTime = fromString . P.show
#endif
{-# INLINE showbNominalDiffTime #-}

-- | Convert a 'AbsoluteTime' into a 'Builder'.
showbAbsoluteTime :: AbsoluteTime -> Builder
showbAbsoluteTime t = showbLocalTime (utcToLocalTime utc $ taiToUTCTime (const 0) t)
                      <> " TAI" -- ugly, but standard apparently
{-# INLINE showbAbsoluteTime #-}

-- | Convert a 'TimeZone' into a 'Builder'.
showbTimeZone :: TimeZone -> Builder
#if defined(TEXT_FORMAT)
showbTimeZone = build
#else
showbTimeZone zone@(TimeZone _ _ "") = timeZoneOffsetBuilder zone
showbTimeZone (TimeZone _ _ name)    = fromString name
#endif
{-# INLINE showbTimeZone #-}

-- | Convert a 'TimeOfDay' into a 'Builder'.
showbTimeOfDay :: TimeOfDay -> Builder
#if defined(TEXT_FORMAT)
showbTimeOfDay = build
#else
showbTimeOfDay (TimeOfDay h m sec) = showb2      zeroOpt h
                                  <> s ':'
                                  <> showb2      zeroOpt m
                                  <> s ':'
                                  <> showb2Fixed zeroOpt sec
#endif
{-# INLINE showbTimeOfDay #-}

-- | Convert a 'LocalTime' into a 'Builder'.
showbLocalTime :: LocalTime -> Builder
#if defined(TEXT_FORMAT)
showbLocalTime = build
#else
showbLocalTime (LocalTime d t) = showbGregorian d <> s ' ' <> showb t
#endif
{-# INLINE showbLocalTime #-}

-- | Convert a 'ZonedTime' into a 'Builder'.
showbZonedTime :: ZonedTime -> Builder
#if defined(TEXT_FORMAT)
showbZonedTime = build
#else
showbZonedTime (ZonedTime t zone) = showb t <> s ' ' <> showb zone
#endif
{-# INLINE showbZonedTime #-}

#if !defined(TEXT_FORMAT)
pad1 :: NumericPadOption -> Builder -> Builder
pad1 (Just c) b = s c <> b
pad1 _        b = b
{-# INLINE pad1 #-}

padN :: Int -> Char -> Builder -> Builder
padN i _ b | i <= 0 = b
padN i c b          = replicateB (fromIntegral i) (s c) <> b
{-# INLINE padN #-}

showb2 :: (Num t, Ord t, Show t) => NumericPadOption -> t -> Builder
showb2 = showbPaddedMin 2
{-# INLINE showb2 #-}

showb2Fixed :: NumericPadOption -> Pico -> Builder
showb2Fixed opt x | x < 10 = pad1 opt $ showbFixed True x
showb2Fixed _   x          = showbFixed True x
{-# INLINE showb2Fixed #-}

showb4 :: (Num t, Ord t, Show t) => NumericPadOption -> t -> Builder
showb4 = showbPaddedMin 4
{-# INLINE showb4 #-}

showbGregorian :: Day -> Builder
showbGregorian date = showb4 zeroOpt y
                   <> s '-'
                   <> showb2 zeroOpt m
                   <> s '-'
                   <> showb2 zeroOpt d
  where
    (y,m,d) = toGregorian date
{-# INLINE showbGregorian #-}

showbPaddedMin :: (Num t, Ord t, Show t) => Int -> NumericPadOption -> t -> Builder
showbPaddedMin _  Nothing  i = showb i
showbPaddedMin pl opt      i | i < 0 = s '-' <> showbPaddedMin pl opt (negate i)
showbPaddedMin pl (Just c) i =
    let b = showb i
    in padN (pl - fromIntegral (lengthB b)) c b
{-# INLINE showbPaddedMin #-}

showbT :: NumericPadOption -> Int -> Builder
showbT opt t = showb4 opt ((div t 60) * 100 + (mod t 60))
{-# INLINE showbT #-}

timeZoneOffsetBuilder' :: NumericPadOption -> TimeZone -> Builder
timeZoneOffsetBuilder' opt (TimeZone t _ _) | t < 0 = s '-' <> showbT opt (negate t)
timeZoneOffsetBuilder' opt (TimeZone t _ _) = s '+' <> showbT opt t
{-# INLINE timeZoneOffsetBuilder' #-}

timeZoneOffsetBuilder :: TimeZone -> Builder
timeZoneOffsetBuilder = timeZoneOffsetBuilder' $ Just '0'
{-# INLINE timeZoneOffsetBuilder #-}

zeroOpt :: NumericPadOption
zeroOpt = Just '0'
{-# INLINE zeroOpt #-}
#endif

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

instance Show ZonedTime where
    showb = showbZonedTime
    {-# INLINE showb #-}