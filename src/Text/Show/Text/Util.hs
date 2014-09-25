-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Show.Text.Util
-- Copyright   :  (C) 2014 Ryan Scott
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  Experimental
-- Portability :  GHC
-- 
-- Utilities used across @text-show@ modules.
----------------------------------------------------------------------------
module Text.Show.Text.Util (s) where

import Data.Text.Lazy.Builder (Builder, singleton)

-- |
-- A shorter name for 'singleton' for convenience's sake (since it tends to be used
-- pretty often in @text-show@).
s :: Char -> Builder
s = singleton