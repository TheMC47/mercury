module Mercury.Window.GeometrySpec (spec) where

import Mercury.Window.Geometry
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = describe "concretizeDimension" $ do
    it "returns the specified dimension with Absolute" $
        property $
            \dim absVal ->
                concretizeDimension dim (Absolute absVal) `shouldBe` absVal
