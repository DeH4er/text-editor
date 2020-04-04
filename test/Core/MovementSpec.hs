module Core.MovementSpec
  (spec
  )
where


import Test.Hspec
import Core.Movement

import qualified Core.Cursor as Cursor
import Core.Cursor (Cursor)


spec = do
  describe "forwardWord" $ do
    it "should move forward by space separated words" $ do
      let
        content = ["abc abc", "abc abc"]
        c1 = Cursor.new 0 0
        c2 = forwardWord content c1
        c3 = forwardWord content c2
        c4 = forwardWord content c3
        c5 = forwardWord content c4
        c6 = forwardWord content c5

      c2 `shouldBe` Cursor.new 0 4
      c3 `shouldBe` Cursor.new 1 0
      c4 `shouldBe` Cursor.new 1 4
      c5 `shouldBe` Cursor.new 1 7
      c6 `shouldBe` Cursor.new 1 7

    it "should ignore spaces after endl" $ do
      let
        content = ["abc abc", "  abc abc"]
        c1 = Cursor.new 0 0
        c2 = forwardWord content c1
        c3 = forwardWord content c2
        c4 = forwardWord content c3
        c5 = forwardWord content c4
        c6 = forwardWord content c5

      c2 `shouldBe` Cursor.new 0 4
      c3 `shouldBe` Cursor.new 1 2
      c4 `shouldBe` Cursor.new 1 6
      c5 `shouldBe` Cursor.new 1 9
      c6 `shouldBe` Cursor.new 1 9

    it "should go to next line if next line is empty" $ do
      let
        content = ["abc abc", "", "  abc abc"]
        c1 = Cursor.new 0 0
        c2 = forwardWord content c1
        c3 = forwardWord content c2
        c4 = forwardWord content c3
        c5 = forwardWord content c4
        c6 = forwardWord content c5
        c7 = forwardWord content c6

      c2 `shouldBe` Cursor.new 0 4
      c3 `shouldBe` Cursor.new 1 0
      c4 `shouldBe` Cursor.new 2 2
      c5 `shouldBe` Cursor.new 2 6
      c6 `shouldBe` Cursor.new 2 9
      c7 `shouldBe` Cursor.new 2 9

  describe "backwardWord" $ do
    it "should move backward by space separated words on single line" $ do
      let
      --content = ["43210 8765 23 0"]
        content = ["0 23 5678 01234"]
        c1 = Cursor.new 0 15
        c2 = backwardWord content c1
        c3 = backwardWord content c2
        c4 = backwardWord content c3
        c5 = backwardWord content c4
        c6 = backwardWord content c5

      c2 `shouldBe` Cursor.new 0 10
      c3 `shouldBe` Cursor.new 0 5
      c4 `shouldBe` Cursor.new 0 2
      c5 `shouldBe` Cursor.new 0 0
      c6 `shouldBe` Cursor.new 0 0

    it "should move backward by space separated words" $ do
      let
        content = ["abc abc", "abc abc"]
        c1 = Cursor.new 1 7
        c2 = backwardWord content c1
        c3 = backwardWord content c2
        c4 = backwardWord content c3
        c5 = backwardWord content c4
        c6 = backwardWord content c5

      c2 `shouldBe` Cursor.new 1 4
      c3 `shouldBe` Cursor.new 1 0
      c4 `shouldBe` Cursor.new 0 4
      c5 `shouldBe` Cursor.new 0 0
      c6 `shouldBe` Cursor.new 0 0
