module Mercury.Runtime.Rendering.GtkSpec (spec) where

import Graphics.X11
import Mercury.Runtime.Rendering.Gtk (concretizeStruts)
import Mercury.Window.Geometry
import Test.Hspec

spec :: Spec
spec = describe "Struts" $ do
    -- Example 2 https://specifications.freedesktop.org/wm/latest/ar01s05.html#id-1.6.11
    it "should set bottom struts correctly on a right smaller screen (top-aligned)" $ do
        let
            screen =
                Rectangle
                    { rect_x = 1280
                    , rect_y = 0
                    , rect_width = 1024
                    , rect_height = 768
                    }
            rootW = 1280 + 1024
            rootH = 1024
        concretizeStruts rootW rootH screen (Strut B (Absolute 50))
            `shouldBe` [ 0
                       , 0
                       , 0
                       , 306
                       , 0
                       , 0
                       , 0
                       , 0
                       , 0
                       , 0
                       , 1280
                       , 2303
                       ]
