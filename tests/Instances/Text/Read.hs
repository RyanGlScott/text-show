{-# LANGUAGE CPP                  #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}

#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE DataKinds            #-}
#endif

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

#if MIN_VERSION_base(4,6,0)
import           Language.Haskell.TH.Lib (conT)
import           TextShow.TH.Names (numberTypeName)
#endif

$(Generics.deriveAll0 ''Lexeme)
#if MIN_VERSION_base(4,6,0)
$(Generics.deriveAll0 numberTypeName)
#endif

instance Arbitrary Lexeme where
    arbitrary = genericArbitrary

#if MIN_VERSION_base(4,6,0)
-- NB: Don't attempt to define
--
-- type Number' = $(conT numberTypeName)
--
-- here. Sadly, due to a bizarre GHC 7.6 bug, it'll think it's a recursive
-- type synonym and reject it.

instance Arbitrary $(conT numberTypeName) where
    arbitrary = genericArbitrary
#endif
