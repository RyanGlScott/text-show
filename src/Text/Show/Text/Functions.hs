{-# LANGUAGE NoImplicitPrelude #-}
module Text.Show.Text.Functions where

import Data.Int (Int64)
import Data.Monoid (Monoid(mempty), (<>))
import Data.Text.Lazy (length, replicate)
import Data.Text.Lazy.Builder (Builder, fromLazyText, singleton, toLazyText)

import Prelude hiding (length, replicate)

-- |
-- A shorter name for 'singleton' for convenience's sake (since it tends to be used
-- pretty often in @text-show@).
s :: Char -> Builder
s = singleton
{-# INLINE s #-}

lengthB :: Builder -> Int64
lengthB = length . toLazyText
{-# INLINE lengthB #-}

replicateB :: Int64 -> Builder -> Builder
replicateB n = fromLazyText . replicate n . toLazyText
{-# INLINE replicateB #-}

-- | Merges several 'Builder's, separating them by newlines.
unlinesB :: [Builder] -> Builder
unlinesB (b:bs) = b <> s '\n' <> unlinesB bs
unlinesB []     = mempty
{-# INLINE unlinesB #-}

-- | Merges several 'Builder's, separating them by spaces.
unwordsB :: [Builder] -> Builder
unwordsB (b:bs@(_:_)) = b <> s ' ' <> unwordsB bs
unwordsB [b]          = b
unwordsB []           = mempty
{-# INLINE unwordsB #-}