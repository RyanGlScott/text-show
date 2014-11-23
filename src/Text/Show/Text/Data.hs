{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Show.Text.Data
-- Copyright   :  (C) 2014 Ryan Scott
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  Experimental
-- Portability :  GHC
-- 
-- Imports 'Show' functions for @Data@ modules.
----------------------------------------------------------------------------
module Text.Show.Text.Data () where

import Text.Show.Text.Data.Array         ()
import Text.Show.Text.Data.Bool          ()
import Text.Show.Text.Data.ByteString    ()
import Text.Show.Text.Data.Char          ()
import Text.Show.Text.Data.Containers    ()
import Text.Show.Text.Data.Data          ()
import Text.Show.Text.Data.Dynamic       ()
import Text.Show.Text.Data.Either        ()
import Text.Show.Text.Data.Fixed         ()
import Text.Show.Text.Data.Floating      ()
import Text.Show.Text.Data.Integral      ()
import Text.Show.Text.Data.List          ()
import Text.Show.Text.Data.Maybe         ()
import Text.Show.Text.Data.Monoid        ()
import Text.Show.Text.Data.Ord           ()
import Text.Show.Text.Data.Text          ()
import Text.Show.Text.Data.Time          ()
import Text.Show.Text.Data.Tuple         ()
#if MIN_VERSION_base(4,7,0)
import Text.Show.Text.Data.Type.Coercion ()
import Text.Show.Text.Data.Type.Equality ()
#endif
import Text.Show.Text.Data.Typeable      ()
import Text.Show.Text.Data.Version       ()