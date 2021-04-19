{-|
Module:      Spec.Text.ReadSpec
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for data types in the "Text.Read" module.
-}
module Spec.Text.ReadSpec (main, spec) where

import Data.Proxy.Compat (Proxy(..))
import Instances.Text.Read ()
import Spec.Utils (matchesTextShowSpec)
import Test.Hspec (Spec, describe, hspec, parallel)
import Text.Read (Lexeme)
import Text.Read.Lex (Number)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "Lexeme" $
        matchesTextShowSpec (Proxy :: Proxy Lexeme)
    describe "Number" $
        matchesTextShowSpec (Proxy :: Proxy Number)
