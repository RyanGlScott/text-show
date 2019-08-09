module Spec.Control.Monad.STSpec (main, spec) where

import Control.Monad.ST
import Data.Proxy.Compat (Proxy(..))
import Instances.Control.Monad.ST ()
import Spec.Utils (matchesTextShowSpec)
import Test.Hspec (Spec, describe, hspec, parallel)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel . describe "ST Int Int" $
    matchesTextShowSpec (Proxy :: Proxy (ST Int Int))
