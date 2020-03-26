module Core.BufferSpec
  ( spec
  )
where

import Test.Hspec
import Core.Buffer
import Core.Cursor


spec = do
  describe "Load file" $ do
    it "should load content" $ do
      let content = ["content"]
      getContent (loadContent emptyBuffer "" content)
        `shouldBe` content

    it "should load filepath" $ do
      let path = "path"
      getFilepath (loadContent emptyBuffer path [])
        `shouldBe` Just path

  describe "Move cursor" $ do
    it "empty buffer position" $ do
      emptyBuffer `cursorIsAt` (0, 0)

    it "should move cursor at specific position" $ do
      testBuffer `cursorIsAt` (0, 0)
      moveCursor (moveAt 3 3) testBuffer `cursorIsAt` (3, 3)

    -- move by 1
    it "should move left by 1" $ do
      moveCursor (moveLeft 1) cursorAt33 `cursorIsAt` (3, 2)

    it "should move right by 1" $ do
      moveCursor (moveRight 1) cursorAt33 `cursorIsAt` (3, 4)

    it "should move top by 1" $ do
      moveCursor (moveTop 1) cursorAt33 `cursorIsAt` (2, 3)

    it "should move bottom by 1" $ do
      moveCursor (moveBottom 1) cursorAt33 `cursorIsAt` (4, 3)

    -- move by n
    it "should move left by n" $ do
      moveCursor (moveLeft 2) cursorAt33 `cursorIsAt` (3, 1)

    it "should move right by n" $ do
      moveCursor (moveRight 2) cursorAt33 `cursorIsAt` (3, 5)

    it "should move top by n" $ do
      moveCursor (moveTop 2) cursorAt33 `cursorIsAt` (1, 3)

    it "should move bottom by n" $ do
      moveCursor (moveBottom 2) cursorAt33 `cursorIsAt` (5, 3)

    -- boundaries
    it "shouldnt move left on left boundary" $ do
      moveCursor (moveLeft 100) cursorAt33 `cursorIsAt` (3, 0)

    it "shouldnt move right on right boundary" $ do
      moveCursor (moveRight 100) cursorAt33 `cursorIsAt` (3, 6)

    it "shouldnt move top on top boundary" $ do
      moveCursor (moveTop 100) cursorAt33 `cursorIsAt` (0, 3)

    it "shouldnt move bottom on bottom boundary" $ do
      moveCursor (moveBottom 100) cursorAt33 `cursorIsAt` (5, 3)

  describe "Insert char" $ do
    it "should insert char in empty buffer" $ do
      insertChar emptyBuffer 'a' `contentShouldBe` ["a"]

    it "should insert char at zero col" $ do
      let buf1 = insertChar testBuffer 'a'
      buf1 `contentShouldBe`
        [ "a123456"
        , "123456"
        , "123456"
        , "123456"
        , "123456"
        , "123456"
        ]
      buf1 `cursorIsAt` (0, 1)

    it "should insert char between zero and max col" $ do
      let buf1 = insertChar cursorAt33 'a'
      buf1 `contentShouldBe`
        [ "123456"
        , "123456"
        , "123456"
        , "123a456"
        , "123456"
        , "123456"
        ]
      buf1 `cursorIsAt` (3, 4)

    it "should insert char at max col" $ do
      let
        buf1 = moveCursor (moveRight 100) testBuffer
        buf2 = insertChar buf1 'a'
      buf2 `contentShouldBe`
        [ "123456a"
        , "123456"
        , "123456"
        , "123456"
        , "123456"
        , "123456"
        ]
      buf2 `cursorIsAt` (0, 7)

  describe "Delete char" $ do
    it "shouldnt delete char at zero col and at zero row" $ do
      pending

    it "should delete new line at zero col but not zero row" $ do
      pending

    it "should delete char at middle of the line" $ do
      pending

    it "should delete char at max col" $ do
      pending

  describe "Break line" $ do
    it "todo" $ do
      pending


cursorIsAt buffer (row, col) = getCursor buffer `shouldBe` mkCursor row col
contentShouldBe buffer content = getContent buffer `shouldBe` content

cursorAt33 = moveCursor (moveAt 3 3) testBuffer
testBuffer = loadContent emptyBuffer "" (replicate 6 "123456")
