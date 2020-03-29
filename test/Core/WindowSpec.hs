module Core.WindowSpec
  ( spec
  )
where


import Test.Hspec
import qualified Core.Buffer as Buffer
import qualified Core.Window as Window
import qualified Core.Window.Rect as Rect


spec = do
  describe "Load buffer" $ do
    it "should load buffer" $ do
      Window.getBuffer testWindow
        `shouldBe` testBuffer

  describe "Resize and translate window" $ do
    it "should resize window" $ do
      Window.getRect (Window.setRect 0 0 20 20 testWindow)
        `shouldBe` Rect.new 0 0 20 20

  describe "Move window cursor" $ do
    it "should move cursor left" $ do
      pending


testWindow :: Window.Window
testWindow =
  Window.setRect 0 0 20 20
  . Window.loadBuffer testBuffer
  $ Window.empty


testBuffer :: Buffer.Buffer
testBuffer =
  Buffer.loadContent Buffer.empty "" $ replicate 6 "123456"
