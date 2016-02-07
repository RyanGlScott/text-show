{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module:      TextShow.Data.Semigroup
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Monomorphic 'TextShow' functions for data types in the "Data.Semigroup" module.

/Since: 3/
-}
module TextShow.Data.Semigroup (
      liftShowbMinPrec
    , liftShowbMaxPrec
    , liftShowbFirstPrec
    , liftShowbLastPrec
    , liftShowbWrappedMonoidPrec
    , liftShowbOptionPrec
    , liftShowbArgPrec2
    ) where

import Data.Semigroup (Min, Max, First, Last, WrappedMonoid, Option, Arg)
import Data.Text.Lazy.Builder (Builder)

import TextShow.Classes (TextShow1(..), TextShow2(..))
import TextShow.Data.Maybe ()
import TextShow.TH.Internal (deriveTextShow, deriveTextShow1, deriveTextShow2)

-- | Convert a 'Min' value to a 'Builder' with the given show function and precedence.
--
-- /Since: 3/
liftShowbMinPrec :: (Int -> a -> Builder) -> Int -> Min a -> Builder
liftShowbMinPrec sp = liftShowbPrec sp undefined
{-# INLINE liftShowbMinPrec #-}

-- | Convert a 'Max' value to a 'Builder' with the given show function and precedence.
--
-- /Since: 3/
liftShowbMaxPrec :: (Int -> a -> Builder) -> Int -> Max a -> Builder
liftShowbMaxPrec sp = liftShowbPrec sp undefined
{-# INLINE liftShowbMaxPrec #-}

-- | Convert a 'First' value to a 'Builder' with the given show function and precedence.
--
-- /Since: 3/
liftShowbFirstPrec :: (Int -> a -> Builder) -> Int -> First a -> Builder
liftShowbFirstPrec sp = liftShowbPrec sp undefined
{-# INLINE liftShowbFirstPrec #-}

-- | Convert a 'Last' value to a 'Builder' with the given show function and precedence.
--
-- /Since: 3/
liftShowbLastPrec :: (Int -> a -> Builder) -> Int -> Last a -> Builder
liftShowbLastPrec sp = liftShowbPrec sp undefined
{-# INLINE liftShowbLastPrec #-}

-- | Convert a 'WrappedMonoid' to a 'Builder' with the given show function
-- and precedence.
--
-- /Since: 3/
liftShowbWrappedMonoidPrec :: (Int -> m -> Builder) -> Int -> WrappedMonoid m -> Builder
liftShowbWrappedMonoidPrec sp = liftShowbPrec sp undefined
{-# INLINE liftShowbWrappedMonoidPrec #-}

-- | Convert an 'Option' value to a 'Builder' with the given show function
-- and precedence.
--
-- /Since: 3/
liftShowbOptionPrec :: (Int -> a -> Builder) -> Int -> Option a -> Builder
liftShowbOptionPrec sp = liftShowbPrec sp undefined
{-# INLINE liftShowbOptionPrec #-}

-- | Convert an 'Arg' value to a 'Builder' with the given show functions and precedence.
--
-- /Since: 3/
liftShowbArgPrec2 :: (Int -> a -> Builder) -> (Int -> b -> Builder)
                  -> Int -> Arg a b -> Builder
liftShowbArgPrec2 sp1 sp2 = liftShowbPrec2 sp1 undefined sp2 undefined
{-# INLINE liftShowbArgPrec2 #-}

$(deriveTextShow  ''Min)
$(deriveTextShow1 ''Min)

$(deriveTextShow  ''Max)
$(deriveTextShow1 ''Max)

$(deriveTextShow  ''First)
$(deriveTextShow1 ''First)

$(deriveTextShow  ''Last)
$(deriveTextShow1 ''Last)

$(deriveTextShow  ''WrappedMonoid)
$(deriveTextShow1 ''WrappedMonoid)

$(deriveTextShow  ''Option)
$(deriveTextShow1 ''Option)

$(deriveTextShow  ''Arg)
$(deriveTextShow1 ''Arg)
$(deriveTextShow2 ''Arg)
