module Core.BufferSpec
  ( spec
  )
where


import Test.Hspec
import Core.MoveAction
import qualified Core.Buffer as Buffer
import qualified Core.Cursor as Cursor


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

  describe "Move cursor" $ do
    it "empty buffer position" $ do
      Buffer.empty `cursorIsAt` (0, 0)

    it "should move cursor at specific position" $ do
      testBuffer `cursorIsAt` (0, 0)
      Buffer.moveCursor (moveAt 3 3) testBuffer `cursorIsAt` (3, 3)

    -- move by 1
    it "should move left by 1" $ do
      Buffer.moveCursor (moveLeft 1) cursorAt33 `cursorIsAt` (3, 2)

    it "should move right by 1" $ do
      Buffer.moveCursor (moveRight 1) cursorAt33 `cursorIsAt` (3, 4)

    it "should move top by 1" $ do
      Buffer.moveCursor (moveTop 1) cursorAt33 `cursorIsAt` (2, 3)

    it "should move bottom by 1" $ do
      Buffer.moveCursor (moveBottom 1) cursorAt33 `cursorIsAt` (4, 3)

    -- move by n
    it "should move left by n" $ do
      Buffer.moveCursor (moveLeft 2) cursorAt33 `cursorIsAt` (3, 1)

    it "should move right by n" $ do
      Buffer.moveCursor (moveRight 2) cursorAt33 `cursorIsAt` (3, 5)

    it "should move top by n" $ do
      Buffer.moveCursor (moveTop 2) cursorAt33 `cursorIsAt` (1, 3)

    it "should move bottom by n" $ do
      Buffer.moveCursor (moveBottom 2) cursorAt33 `cursorIsAt` (5, 3)

    -- boundaries
    it "shouldnt move left on left boundary" $ do
      Buffer.moveCursor (moveLeft 100) cursorAt33 `cursorIsAt` (3, 0)

    it "shouldnt move right on right boundary" $ do
      Buffer.moveCursor (moveRight 100) cursorAt33 `cursorIsAt` (3, 6)

    it "shouldnt move top on top boundary" $ do
      Buffer.moveCursor (moveTop 100) cursorAt33 `cursorIsAt` (0, 3)

    it "shouldnt move bottom on bottom boundary" $ do
      Buffer.moveCursor (moveBottom 100) cursorAt33 `cursorIsAt` (5, 3)

  describe "Insert char" $ do
    it "should insert char in empty buffer" $ do
      Buffer.insertChar Buffer.empty 'a' `contentShouldBe` ["a"]

    it "should insert char at zero col" $ do
      let buf1 = Buffer.insertChar testBuffer 'a'
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
      let buf1 = Buffer.insertChar cursorAt33 'a'
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
        buf1 = Buffer.moveCursor (moveRight 100) testBuffer
        buf2 = Buffer.insertChar buf1 'a'
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
      Buffer.deleteChar testBuffer `contentShouldBe` (Buffer.getContent testBuffer)

    it "should delete new line at zero col but not zero row" $ do
      let buf1 = Buffer.moveCursor (moveAt 1 0) testBuffer
      Buffer.deleteChar buf1
        `contentShouldBe`
        [ "123456123456"
        , "123456"
        , "123456"
        , "123456"
        , "123456"
        ]

    it "should delete char at middle of the line" $ do
      let buf1 = Buffer.moveCursor (moveAt 1 1) testBuffer
      Buffer.deleteChar buf1
        `contentShouldBe`
        [ "123456"
        , "23456"
        , "123456"
        , "123456"
        , "123456"
        , "123456"
        ]

    it "should delete char at max col" $ do
      let buf1 = Buffer.moveCursor (moveAt 1 1) testBuffer
          buf2 = Buffer.moveCursor (moveRight 100) buf1
      Buffer.deleteChar buf2
        `contentShouldBe`
        [ "123456"
        , "12345"
        , "123456"
        , "123456"
        , "123456"
        , "123456"
        ]

  describe "Break line" $ do
    it "should break line for empty buffer" $ do
      Buffer.breakLine Buffer.empty
        `contentShouldBe`
        ["",""]

    it "should break line at zero col" $ do
      let buf1 = Buffer.moveCursor (moveAt 2 0) testBuffer
      Buffer.breakLine buf1
        `contentShouldBe`
        [ "123456"
        , "123456"
        , ""
        , "123456"
        , "123456"
        , "123456"
        , "123456"
        ]

    it "should break line at middle of the line" $ do
      Buffer.breakLine cursorAt33
        `contentShouldBe`
        [ "123456"
        , "123456"
        , "123456"
        , "123"
        , "456"
        , "123456"
        , "123456"
        ]

    it "should break line at max col" $ do
      let buf1 = Buffer.moveCursor (moveAt 2 0) testBuffer
          buf2 = Buffer.moveCursor (moveRight 100) buf1
      Buffer.breakLine buf2
        `contentShouldBe`
        [ "123456"
        , "123456"
        , "123456"
        , ""
        , "123456"
        , "123456"
        , "123456"
        ]


cursorIsAt buffer (row, col) = Buffer.getCursor buffer `shouldBe` Cursor.new row col
contentShouldBe buffer content = Buffer.getContent buffer `shouldBe` content

cursorAt33 = Buffer.moveCursor (moveAt 3 3) testBuffer
testBuffer = Buffer.loadContent Buffer.empty "" (replicate 6 "123456")
