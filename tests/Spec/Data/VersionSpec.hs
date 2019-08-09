module Spec.Data.VersionSpec (main, spec) where

import Data.Proxy.Compat (Proxy(..))
import Data.Version (Version, showVersion)

import Spec.Utils (matchesTextShowSpec)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

import TextShow (fromString)
import TextShow.Data.Version (showbVersion)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "Version" $
        matchesTextShowSpec (Proxy :: Proxy Version)
    describe "showbVersion" $
        prop "has the same output as showVersion" prop_showVersion

-- | Verifies 'showVersion' and 'showbVersion' generate the same output.
prop_showVersion :: Version -> Bool
prop_showVersion v = fromString (showVersion v) == showbVersion v
