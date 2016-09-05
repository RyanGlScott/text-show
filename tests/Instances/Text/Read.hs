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
Copyright:   (C) 2014-2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'Arbitrary' instances for data types in the "Text.Read" module.
-}
module Instances.Text.Read (
#if MIN_VERSION_base(4,6,0)
    Number'
#endif
    ) where

import qualified Generics.Deriving.TH as Generics (deriveAll0)
#if MIN_VERSION_base(4,6,0)
import           Language.Haskell.TH.Lib (conT)
#endif
import           Test.QuickCheck (Arbitrary(..), genericArbitrary)
import           Text.Read (Lexeme(..))
import           TextShow.TH.Names (numberTypeName)

instance Arbitrary Lexeme where
    arbitrary = genericArbitrary

#if MIN_VERSION_base(4,6,0)
type Number' = $(conT numberTypeName)

instance Arbitrary Number' where
    arbitrary = genericArbitrary
#endif

$(Generics.deriveAll0 ''Lexeme)
$(Generics.deriveAll0 numberTypeName)
