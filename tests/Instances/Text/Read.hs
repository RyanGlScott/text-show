{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module:      Instances.Text.Read
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'Arbitrary' instances for data types in the "Text.Read" module.
-}
module Instances.Text.Read () where

import qualified Generics.Deriving.TH as Generics (deriveAll0)
import           Instances.Utils.GenericArbitrary (genericArbitrary)
import           Test.QuickCheck (Arbitrary(..))
import           Text.Read (Lexeme(..))
import           Text.Read.Lex (Number)

$(Generics.deriveAll0 ''Lexeme)
$(Generics.deriveAll0 ''Number)

instance Arbitrary Lexeme where
    arbitrary = genericArbitrary

instance Arbitrary Number where
    arbitrary = genericArbitrary
