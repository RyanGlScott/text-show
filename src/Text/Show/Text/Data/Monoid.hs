{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Text.Show.Text.Data.Monoid where

import Data.Monoid (Any(..), All(..), First(..), Last(..),
                    Product(..), Sum(..), (<>))
import Data.Text.Lazy.Builder (Builder)

import GHC.Show (appPrec)

import Prelude hiding (Show)

import Text.Show.Text.Class (Show)

