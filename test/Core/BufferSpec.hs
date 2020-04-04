module Core.BufferSpec
  ( spec
  )
where


import Test.Hspec
import qualified Core.Buffer as Buffer


spec = do
  describe "Load file" $ do
    it "should load content" $ do
      let content = ["content"]
      Buffer.getContent (Buffer.loadContent Buffer.empty "" content)
        `shouldBe` content

    it "should load filepath" $ do
      let path = "path"
      Buffer.getFilepath (Buffer.loadContent Buffer.empty path [])
        `shouldBe` Just path
