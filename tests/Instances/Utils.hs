{-|
Module:      Instances.Utils
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

A collection of utilities.
-}
module Instances.Utils (GenericExample(..), (<@>)) where

-- | A simple data type for testing if 'FromGeneric' and
-- 'FromGeneric1' work as intended.
data GenericExample a = GE1 a (Maybe a) (Maybe (Maybe a))
                      | GE2
                      | GE3 { ge3 :: a }
                      | a :!@#$: a

infixl 4 <@>
-- | A useful way to escape a 'Functor' context.
(<@>) :: Functor f => f (a -> b) -> a -> f b
f <@> x = fmap ($ x) f
