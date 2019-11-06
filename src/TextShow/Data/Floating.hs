{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module:      TextShow.Data.Floating
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'TextShow' instances and monomorphic functions for floating-point types.

/Since: 2/
-}
module TextShow.Data.Floating (
      showbRealFloatPrec
    , showbEFloat
    , showbFFloat
    , showbGFloat
    , showbFFloatAlt
    , showbGFloatAlt
    , showbFPFormat
    , FPFormat(..)
    , formatRealFloatB
    , formatRealFloatAltB
    ) where

import           Data.Array.Base (unsafeAt)
import           Data.Array.IArray (Array, array)
import qualified Data.Text as T (replicate)
import           Data.Text.Lazy.Builder (Builder, fromString, fromText, singleton)
import           Data.Text.Lazy.Builder.Int (decimal)
import           Data.Text.Lazy.Builder.RealFloat (FPFormat(..))

import           Prelude ()
import           Prelude.Compat

import           TextShow.Classes (TextShow(..), showbParen)
import           TextShow.TH.Internal (deriveTextShow)
import           TextShow.Utils (i2d)

-------------------------------------------------------------------------------
-- TextShow instances
-------------------------------------------------------------------------------

-- | /Since: 2/
$(deriveTextShow ''FPFormat)

-- | /Since: 2/
instance TextShow Float where
    showbPrec = showbRealFloatPrec
    {-# INLINE showbPrec #-}

-- | /Since: 2/
instance TextShow Double where
    showbPrec = showbRealFloatPrec
    {-# INLINE showbPrec #-}

-------------------------------------------------------------------------------
-- Standalone showb* functions
-------------------------------------------------------------------------------

-- | Convert a 'RealFloat' value to a 'Builder' with the given precedence.
--
-- /Since: 2/
showbRealFloatPrec :: RealFloat a => Int -> a -> Builder
showbRealFloatPrec p x
    | x < 0 || isNegativeZero x = showbParen (p > 6) $ singleton '-' <> showbGFloat Nothing (-x)
    | otherwise                 = showbGFloat Nothing x
{-# INLINE showbRealFloatPrec #-}

{-# SPECIALIZE showbEFloat ::
        Maybe Int -> Float  -> Builder,
        Maybe Int -> Double -> Builder #-}
{-# SPECIALIZE showbFFloat ::
        Maybe Int -> Float  -> Builder,
        Maybe Int -> Double -> Builder #-}
{-# SPECIALIZE showbGFloat ::
        Maybe Int -> Float  -> Builder,
        Maybe Int -> Double -> Builder #-}

-- | Show a signed 'RealFloat' value
-- using scientific (exponential) notation (e.g. @2.45e2@, @1.5e-3@).
--
-- In the call @'showbEFloat' digs val@, if @digs@ is 'Nothing',
-- the value is shown to full precision; if @digs@ is @'Just' d@,
-- then at most @d@ digits after the decimal point are shown.
--
-- /Since: 2/
showbEFloat :: RealFloat a => Maybe Int -> a -> Builder
showbEFloat = formatRealFloatB Exponent

-- | Show a signed 'RealFloat' value
-- using standard decimal notation (e.g. @245000@, @0.0015@).
--
-- In the call @'showbFFloat' digs val@, if @digs@ is 'Nothing',
-- the value is shown to full precision; if @digs@ is @'Just' d@,
-- then at most @d@ digits after the decimal point are shown.
--
-- /Since: 2/
showbFFloat :: RealFloat a => Maybe Int -> a -> Builder
showbFFloat = formatRealFloatB Fixed

-- | Show a signed 'RealFloat' value
-- using standard decimal notation for arguments whose absolute value lies
-- between @0.1@ and @9,999,999@, and scientific notation otherwise.
--
-- In the call @'showbGFloat' digs val@, if @digs@ is 'Nothing',
-- the value is shown to full precision; if @digs@ is @'Just' d@,
-- then at most @d@ digits after the decimal point are shown.
--
-- /Since: 2/
showbGFloat :: RealFloat a => Maybe Int -> a -> Builder
showbGFloat = formatRealFloatB Generic

-- | Show a signed 'RealFloat' value
-- using standard decimal notation (e.g. @245000@, @0.0015@).
--
-- This behaves as 'showFFloat', except that a decimal point
-- is always guaranteed, even if not needed.
--
-- /Since: 2/
showbFFloatAlt :: RealFloat a => Maybe Int -> a -> Builder
showbFFloatAlt d = formatRealFloatAltB Fixed d True
{-# INLINE showbFFloatAlt #-}

-- | Show a signed 'RealFloat' value
-- using standard decimal notation for arguments whose absolute value lies
-- between @0.1@ and @9,999,999@, and scientific notation otherwise.
--
-- This behaves as 'showFFloat', except that a decimal point
-- is always guaranteed, even if not needed.
--
-- /Since: 2/
showbGFloatAlt :: RealFloat a => Maybe Int -> a -> Builder
showbGFloatAlt d = formatRealFloatAltB Generic d True
{-# INLINE showbGFloatAlt #-}

-- | Convert an 'FPFormat' value to a 'Builder'.
--
-- /Since: 2/
showbFPFormat :: FPFormat -> Builder
showbFPFormat = showb
{-# INLINE showbFPFormat #-}

-------------------------------------------------------------------------------
-- GHC.Float internal functions, adapted for Builders
-------------------------------------------------------------------------------

-- | Like 'formatRealFloatAltB', except that the decimal is only shown for arguments
-- whose absolute value is between @0.1@ and @9,999,999@.
--
-- /Since: 2/
formatRealFloatB :: RealFloat a
                 => FPFormat  -- ^ What notation to use.
                 -> Maybe Int -- ^ Number of decimal places to render.
                 -> a
                 -> Builder
formatRealFloatB fmt decs = formatRealFloatAltB fmt decs False
{-# INLINE formatRealFloatB #-}

-- | Converts a 'RealFloat' value to a Builder, specifying if a decimal point
-- should always be shown.
--
-- /Since: 2/
formatRealFloatAltB :: RealFloat a
                    => FPFormat  -- ^ What notation to use.
                    -> Maybe Int -- ^ Number of decimal places to render.
                    -> Bool      -- ^ Should a decimal point always be shown?
                    -> a
                    -> Builder
{-# SPECIALIZE formatRealFloatAltB :: FPFormat -> Maybe Int -> Bool -> Float  -> Builder #-}
{-# SPECIALIZE formatRealFloatAltB :: FPFormat -> Maybe Int -> Bool -> Double -> Builder #-}
formatRealFloatAltB fmt decs alt x
   | isNaN x                   = "NaN"
   | isInfinite x              = if x < 0 then "-Infinity" else "Infinity"
   | x < 0 || isNegativeZero x = singleton '-' <> doFmt fmt (floatToDigits (-x))
   | otherwise                 = doFmt fmt (floatToDigits x)
 where
  doFmt format (is, e) =
    let ds = map i2d is in
    case format of
     Generic ->
      doFmt (if e < 0 || e > 7 then Exponent else Fixed)
            (is,e)
     Exponent ->
      case decs of
       Nothing ->
        let show_e' = decimal (e-1) in
        case ds of
          "0"     -> "0.0e0"
          [d]     -> singleton d <> ".0e" <> show_e'
          (d:ds') -> singleton d <> singleton '.' <> fromString ds' <> singleton 'e' <> show_e'
          []      -> error "formatRealFloat/doFmt/Exponent: []"
       Just d | d <= 0 ->
        -- handle this case specifically since we need to omit the
        -- decimal point as well (#15115).
        -- Note that this handles negative precisions as well for consistency
        -- (see #15509).
        case is of
          [0] -> "0e0"
          _ ->
           let
             (ei,is') = roundTo 1 is
             n:_ = map i2d (if ei > 0 then init is' else is')
           in singleton n <> singleton 'e' <> decimal (e-1+ei)
       Just dec ->
        let dec' = max dec 1 in
        case is of
         [0] -> "0." <> fromText (T.replicate dec' "0") <> "e0"
         _ ->
          let
           (ei,is') = roundTo (dec'+1) is
           (d:ds') = map i2d (if ei > 0 then init is' else is')
          in
          singleton d <> singleton '.' <> fromString ds' <> singleton 'e' <> decimal (e-1+ei)
     Fixed ->
      let
       mk0 ls = case ls of { "" -> "0" ; _ -> fromString ls}
      in
      case decs of
       Nothing
          | e <= 0    -> "0." <> fromText (T.replicate (-e) "0") <> fromString ds
          | otherwise ->
             let
                f 0 str    rs  = mk0 (reverse str) <> singleton '.' <> mk0 rs
                f n str    ""  = f (n-1) ('0':str) ""
                f n str (r:rs) = f (n-1) (r:str) rs
             in
                f e "" ds
       Just dec ->
        let dec' = max dec 0 in
        if e >= 0 then
         let
          (ei,is') = roundTo (dec' + e) is
          (ls,rs)  = splitAt (e+ei) (map i2d is')
         in
         mk0 ls <> (if null rs && not alt then "" else singleton '.' <> fromString rs)
        else
         let
          (ei,is') = roundTo dec' (replicate (-e) 0 ++ is)
          d:ds' = map i2d (if ei > 0 then is' else 0:is')
         in
         singleton d <> (if null ds' && not alt then "" else singleton '.' <> fromString ds')

-- Based on "Printing Floating-Point Numbers Quickly and Accurately"
-- by R.G. Burger and R.K. Dybvig in PLDI 96.
-- This version uses a much slower logarithm estimator. It should be improved.

-- | 'floatToDigits' takes a base and a non-negative 'RealFloat' number,
-- and returns a list of digits and an exponent.
-- In particular, if @x>=0@, and
--
-- > floatToDigits base x = ([d1,d2,...,dn], e)
--
-- then
--
--      (1) @n >= 1@
--
--      (2) @x = 0.d1d2...dn * (base**e)@
--
--      (3) @0 <= di <= base-1@
floatToDigits :: (RealFloat a) => a -> ([Int], Int)
{-# SPECIALIZE floatToDigits :: Float -> ([Int], Int) #-}
{-# SPECIALIZE floatToDigits :: Double -> ([Int], Int) #-}
floatToDigits 0 = ([0], 0)
floatToDigits x =
 let
  (f0, e0) = decodeFloat x
  (minExp0, _) = floatRange x
  p = floatDigits x
  b = floatRadix x
  minExp = minExp0 - p -- the real minimum exponent
  -- Haskell requires that f be adjusted so denormalized numbers
  -- will have an impossibly low exponent.  Adjust for this.
  (f, e) =
   let n = minExp - e0 in
   if n > 0 then (f0 `quot` (expt b n), e0+n) else (f0, e0)
  (r, s', mUp, mDn) =
   if e >= 0 then
    let be = expt b e in
    if f == expt b (p-1) then
      (f*be*b*2, 2*b, be*b, be)     -- according to Burger and Dybvig
    else
      (f*be*2, 2, be, be)
   else
    if e > minExp && f == expt b (p-1) then
      (f*b*2, expt b (-e+1)*2, b, 1)
    else
      (f*2, expt b (-e)*2, 1, 1)
  k :: Int
  k =
   let
    k0 :: Int
    k0 =
     if b == 2 then
        -- logBase 10 2 is very slightly larger than 8651/28738
        -- (about 5.3558e-10), so if log x >= 0, the approximation
        -- k1 is too small, hence we add one and need one fixup step less.
        -- If log x < 0, the approximation errs rather on the high side.
        -- That is usually more than compensated for by ignoring the
        -- fractional part of logBase 2 x, but when x is a power of 1/2
        -- or slightly larger and the exponent is a multiple of the
        -- denominator of the rational approximation to logBase 10 2,
        -- k1 is larger than logBase 10 x. If k1 > 1 + logBase 10 x,
        -- we get a leading zero-digit we don't want.
        -- With the approximation 3/10, this happened for
        -- 0.5^1030, 0.5^1040, ..., 0.5^1070 and values close above.
        -- The approximation 8651/28738 guarantees k1 < 1 + logBase 10 x
        -- for IEEE-ish floating point types with exponent fields
        -- <= 17 bits and mantissae of several thousand bits, earlier
        -- convergents to logBase 10 2 would fail for long double.
        -- Using quot instead of div is a little faster and requires
        -- fewer fixup steps for negative lx.
        let lx = p - 1 + e0
            k1 = (lx * 8651) `quot` 28738
        in if lx >= 0 then k1 + 1 else k1
     else
        -- f :: Integer, log :: Float -> Float,
        --               ceiling :: Float -> Int
        ceiling ((log (fromInteger (f+1) :: Float) +
                 fromIntegral e * log (fromInteger b)) /
                   log 10)
--WAS:            fromInt e * log (fromInteger b))

    fixup n =
      if n >= 0 then
        if r + mUp <= expt 10 n * s' then n else fixup (n+1)
      else
        if expt 10 (-n) * (r + mUp) <= s' then n else fixup (n+1)
   in
   fixup k0

  gen ds rn sN mUpN mDnN =
   let
    (dn, rn') = (rn * 10) `quotRem` sN
    mUpN' = mUpN * 10
    mDnN' = mDnN * 10
   in
   case (rn' < mDnN', rn' + mUpN' > sN) of
    (True,  False) -> dn : ds
    (False, True)  -> dn+1 : ds
    (True,  True)  -> if rn' * 2 < sN then dn : ds else dn+1 : ds
    (False, False) -> gen (dn:ds) rn' sN mUpN' mDnN'

  rds =
   if k >= 0 then
      gen [] r (s' * expt 10 k) mUp mDn
   else
     let bk = expt 10 (-k) in
     gen [] (r * bk) s' (mUp * bk) (mDn * bk)
 in
 (map fromIntegral (reverse rds), k)

roundTo :: Int -> [Int] -> (Int,[Int])
#if MIN_VERSION_base(4,6,0)
roundTo d is =
  case f d True is of
    x@(0,_) -> x
    (1,xs)  -> (1, 1:xs)
    _       -> error "roundTo: bad Value"
 where
  b2 = base `quot` 2

  f n _ []     = (0, replicate n 0)
  f 0 e (x:xs) | x == b2 && e && all (== 0) xs = (0, [])   -- Round to even when at exactly half the base
               | otherwise = (if x >= b2 then 1 else 0, [])
  f n _ (i:xs)
     | i' == base = (1,0:ds)
     | otherwise  = (0,i':ds)
      where
       (c,ds) = f (n-1) (even i) xs
       i'     = c + i
  base = 10
#else
roundTo d is =
  case f d is of
    x@(0,_) -> x
    (1,xs)  -> (1, 1:xs)
    _       -> error "roundTo: bad Value"
 where
  f n []     = (0, replicate n 0)
  f 0 (x:_)  = (if x >= 5 then 1 else 0, [])
  f n (i:xs)
     | i' == 10  = (1,0:ds)
     | otherwise = (0,i':ds)
      where
       (c,ds) = f (n-1) xs
       i'     = c + i
#endif

-- Exponentiation with a cache for the most common numbers.

-- | The minimum exponent in the cache.
minExpt :: Int
minExpt = 0

-- | The maximum exponent (of 2) in the cache.
maxExpt :: Int
maxExpt = 1100

-- | Exponentiate an 'Integer', using a cache if possible.
expt :: Integer -> Int -> Integer
expt base n
    | base == 2 && n >= minExpt && n <= maxExpt = expts `unsafeAt` n
    | base == 10 && n <= maxExpt10              = expts10 `unsafeAt` n
    | otherwise                                 = base^n

-- | Cached powers of two.
expts :: Array Int Integer
expts = array (minExpt,maxExpt) [(n,2^n) | n <- [minExpt .. maxExpt]]

-- | The maximum exponent (of 10) in the cache.
maxExpt10 :: Int
maxExpt10 = 324

-- | Cached powers of 10.
expts10 :: Array Int Integer
expts10 = array (minExpt,maxExpt10) [(n,10^n) | n <- [minExpt .. maxExpt10]]
