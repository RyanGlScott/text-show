{-|
Module:      Instances.Utils
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

A collection of utility functions.
-}
module Instances.Utils ((<@>)) where

infixl 4 <@>
-- | A useful way to escape a 'Functor' context.
(<@>) :: Functor f => f (a -> b) -> a -> f b
f <@> x = fmap ($ x) f
