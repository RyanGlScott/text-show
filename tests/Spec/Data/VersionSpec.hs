module Spec.Data.VersionSpec (main, spec) where

import Data.Version (Version, showVersion)

import Instances.Data.Version ()

import Spec.Utils (prop_matchesShow)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.Hspec.QuickCheck (prop)

import Text.Show.Text (fromString)
import Text.Show.Text.Data.Version (showbVersionConcrete)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel . describe "Text.Show.Text.Data.Version" $ do
    prop "Version instance"            (prop_matchesShow :: Int -> Version -> Bool)
    prop "showbVersionConcrete output" prop_showVersion

-- | Verifies 'showVersion' and 'showbVersion' generate the same output.
prop_showVersion :: Version -> Bool
prop_showVersion v = fromString (showVersion v) == showbVersionConcrete v
