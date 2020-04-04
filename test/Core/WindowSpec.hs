module Core.WindowSpec
  ( spec
  )
where


import Test.Hspec

import qualified Core.Cursor as Cursor

import qualified Core.Buffer as Buffer
import Core.Buffer (Buffer)

import qualified Core.Window as Window
import Core.Window (Window)

import qualified Core.Window.Rect as Rect

import Core.Movement


spec = do
  describe "Load buffer" $ do
    it "should load buffer" $ do
      Window.getBuffer testWindow
        `shouldBe` testBuffer

  describe "Resize and translate window" $ do
    it "should resize window" $ do
      Window.getRect (Window.setRect (Rect.new 0 0 20 20) testWindow)
        `shouldBe` Rect.new 0 0 20 20

  describe "Move single window cursor" $ do
    context "in middle of text" $ do
      it "move left" $ do
        Window.moveCursors MLeft testWindow1CursorMiddle
          `shouldHaveCursor` Cursor.new 3 2

      it "move right" $ do
        Window.moveCursors MRight testWindow1CursorMiddle
          `shouldHaveCursor` Cursor.new 3 4

      it "move top" $ do
        Window.moveCursors MTop testWindow1CursorMiddle
          `shouldHaveCursor` Cursor.new 2 3

      it "move bottom" $ do
        Window.moveCursors MBottom testWindow1CursorMiddle
          `shouldHaveCursor` Cursor.new 4 3

    context "at borders" $ do
      it "move left" $ do
        Window.moveCursors MLeft testWindow
          `shouldHaveCursor` Cursor.new 0 0

      it "move right" $ do
        Window.moveCursors MRight (Window.modifyCursors (const $ [Cursor.new 0 6]) testWindow)
          `shouldHaveCursor` Cursor.new 0 6

      it "move top" $ do
        Window.moveCursors MTop testWindow
          `shouldHaveCursor` Cursor.new 0 0

      it "move bottom" $ do
        Window.moveCursors MBottom (Window.modifyCursors (const $ [Cursor.new 5 0]) testWindow)
          `shouldHaveCursor` Cursor.new 5 0

  describe "Move three window cursors" $ do
    context "in middle of text" $ do
      it "move left" $ do
        let win = Window.moveCursors MLeft testWindow3CursorsMiddle
        length (Window.getAllCursors win) `shouldBe` 3
        win `shouldHaveCursor` Cursor.new 4 2
        win `shouldHaveCursor` Cursor.new 3 2
        win `shouldHaveCursor` Cursor.new 2 2

      it "move right" $ do
        let win = Window.moveCursors MRight testWindow3CursorsMiddle
        length (Window.getAllCursors win) `shouldBe` 3
        win `shouldHaveCursor` Cursor.new 4 4
        win `shouldHaveCursor` Cursor.new 3 4
        win `shouldHaveCursor` Cursor.new 2 4

      it "move top" $ do
        let win = Window.moveCursors MTop testWindow3CursorsMiddle
        length (Window.getAllCursors win) `shouldBe` 3
        win `shouldHaveCursor` Cursor.new 3 3
        win `shouldHaveCursor` Cursor.new 2 3
        win `shouldHaveCursor` Cursor.new 1 3

      it "move bottom" $ do
        let win = Window.moveCursors MBottom testWindow3CursorsMiddle
        length (Window.getAllCursors win) `shouldBe` 3
        win `shouldHaveCursor` Cursor.new 5 3
        win `shouldHaveCursor` Cursor.new 4 3
        win `shouldHaveCursor` Cursor.new 3 3

    context "splash at same position" $ do
      it "splash at left edge" $ do
        let
          win1 = Window.modifyCursors (const $ [Cursor.new 1 0, Cursor.new 1 1, Cursor.new 1 2]) testWindow
          win2 = Window.moveCursors MLeft win1
          win3 = Window.moveCursors MLeft win2

        length (Window.getAllCursors win2) `shouldBe` 2
        win2 `shouldHaveCursor` Cursor.new 1 0
        win2 `shouldHaveCursor` Cursor.new 1 1

        length (Window.getAllCursors win3) `shouldBe` 1
        win3 `shouldHaveCursor` Cursor.new 1 0

      it "splash at right edge" $ do
        let
          win1 = Window.modifyCursors (const $ [Cursor.new 1 6, Cursor.new 1 5, Cursor.new 1 4]) testWindow
          win2 = Window.moveCursors MRight win1
          win3 = Window.moveCursors MRight win2

        length (Window.getAllCursors win2) `shouldBe` 2
        win2 `shouldHaveCursor` Cursor.new 1 6
        win2 `shouldHaveCursor` Cursor.new 1 5

        length (Window.getAllCursors win3) `shouldBe` 1
        win3 `shouldHaveCursor` Cursor.new 1 6

      it "splash at top edge" $ do
        let
          win1 = Window.modifyCursors (const $ [Cursor.new 0 3, Cursor.new 1 3, Cursor.new 2 3]) testWindow
          win2 = Window.moveCursors MTop win1
          win3 = Window.moveCursors MTop win2

        length (Window.getAllCursors win2) `shouldBe` 2
        win2 `shouldHaveCursor` Cursor.new 0 3
        win2 `shouldHaveCursor` Cursor.new 1 3

        length (Window.getAllCursors win3) `shouldBe` 1
        win3 `shouldHaveCursor` Cursor.new 0 3

      it "splash at bottom edge" $ do
        let
          win1 = Window.modifyCursors (const $ [Cursor.new 5 3, Cursor.new 4 3, Cursor.new 3 3]) testWindow
          win2 = Window.moveCursors MBottom win1
          win3 = Window.moveCursors MBottom win2

        length (Window.getAllCursors win2) `shouldBe` 2
        win2 `shouldHaveCursor` Cursor.new 5 3
        win2 `shouldHaveCursor` Cursor.new 4 3

        length (Window.getAllCursors win3) `shouldBe` 1
        win3 `shouldHaveCursor` Cursor.new 5 3

  describe "Break line single cursor" $ do
    it "at end of row" $ do
      let
        win1 = Window.modifyCursors (const $ [Cursor.new 0 6]) testWindow
        win2 = Window.breakLine win1

      win2
        `contentShouldBe`
        [ "123456"
        , ""
        , "123456"
        , "123456"
        , "123456"
        , "123456"
        , "123456"
        ]

      win2 `shouldHaveCursor` Cursor.new 1 0

    it "in middle of row" $ do
      let
        win1 = Window.modifyCursors (const $ [Cursor.new 0 4]) testWindow
        win2 = Window.breakLine win1

      win2
        `contentShouldBe`
        [ "1234"
        , "56"
        , "123456"
        , "123456"
        , "123456"
        , "123456"
        , "123456"
        ]

      win2 `shouldHaveCursor` Cursor.new 1 0

  describe "Break line three cursors" $ do
    context "different rows" $ do
      it "at end of row" $ do
        let
          win1 = Window.modifyCursors (const $ [Cursor.new 2 6, Cursor.new 1 6, Cursor.new 0 6]) testWindow
          win2 = Window.breakLine win1

        win2
          `contentShouldBe`
          [ "123456"
          , ""
          , "123456"
          , ""
          , "123456"
          , ""
          , "123456"
          , "123456"
          , "123456"
          ]

        win2 `shouldHaveCursor` Cursor.new 1 0
        win2 `shouldHaveCursor` Cursor.new 3 0
        win2 `shouldHaveCursor` Cursor.new 5 0

      it "in middle of row" $ do
        pending

    context "some on same row" $ do
      it "" $ do
        pending

shouldHaveCursor window cursor =
  (cursor `elem` Window.getAllCursors window) `shouldBe` True


contentShouldBe =
  shouldBe . Window.getContent


testWindow3CursorsMiddle :: Window
testWindow3CursorsMiddle =
  Window.modifyCursors (const $ [Cursor.new 3 3, Cursor.new 2 3, Cursor.new 4 3])
  testWindow


testWindow1CursorMiddle :: Window
testWindow1CursorMiddle =
  Window.modifyCursors (const $ [Cursor.new 3 3])
  testWindow


testWindow :: Window
testWindow =
  Window.setRect (Rect.new 0 0 20 20)
  . Window.fromBuffer
  $ testBuffer


testBuffer :: Buffer
testBuffer =
  Buffer.loadContent Buffer.empty "" $ replicate 6 "123456"
