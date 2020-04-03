module Core.CursorUtilsSpec
  (spec
  )
where


import Test.Hspec
import Core.CursorUtils

import qualified Core.Cursor as Cursor
import Core.Cursor (Cursor)


spec = do
  describe "moveForwardWord" $ do
    it "should move forward by space separated words" $ do
      let
        content = ["abc abc", "abc abc"]
        c1 = Cursor.new 0 0
        c2 = moveForwardWord content c1
        c3 = moveForwardWord content c2
        c4 = moveForwardWord content c3
        c5 = moveForwardWord content c4
        c6 = moveForwardWord content c5

      c2 `shouldBe` Cursor.new 0 4
      c3 `shouldBe` Cursor.new 1 0
      c4 `shouldBe` Cursor.new 1 4
      c5 `shouldBe` Cursor.new 1 7
      c6 `shouldBe` Cursor.new 1 7

    it "should ignore spaces after endl" $ do
      let
        content = ["abc abc", "  abc abc"]
        c1 = Cursor.new 0 0
        c2 = moveForwardWord content c1
        c3 = moveForwardWord content c2
        c4 = moveForwardWord content c3
        c5 = moveForwardWord content c4
        c6 = moveForwardWord content c5

      c2 `shouldBe` Cursor.new 0 4
      c3 `shouldBe` Cursor.new 1 2
      c4 `shouldBe` Cursor.new 1 6
      c5 `shouldBe` Cursor.new 1 9
      c6 `shouldBe` Cursor.new 1 9
