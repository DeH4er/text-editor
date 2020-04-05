module Core.DeletionSpec
  ( spec
  )
where


import Test.Hspec
import qualified Core.Deletion as Deletion

import qualified Core.Cursor as Cursor
import Core.Cursor (Cursor)


spec = do
  describe "deleteBetweenCursors" $ do
    it "should delete on single line" $ do
      let
        content =
          ["0123456789"]

        cursors =
          Cursor.new 0 <$> [0 .. 10]

        del start end =
          Deletion.deleteBetweenCursors (cursors !! start) (cursors !! end) content

      del 0 9 `shouldBe` [""]
      del 1 8 `shouldBe` ["09"]
      del 2 7 `shouldBe` ["0189"]
      del 3 6 `shouldBe` ["012789"]
      del 4 5 `shouldBe` ["01236789"]
      del 5 5 `shouldBe` ["012346789"]

    it "should delete on multiple lines" $ do
      let
        content =
          [ "0123456789"  -- 0
          , "0123456789"  -- 1
          , "0123456789"  -- 2
          , "0123456789"  -- 3
          ]

        del (startRow, startCol) (endRow, endCol) =
          Deletion.deleteBetweenCursors (Cursor.new startRow startCol) (Cursor.new endRow endCol) content

      del (0, 0) (3, 8) `shouldBe` ["9"]
      del (0, 1) (3, 8) `shouldBe` ["09"]

      del (0, 0) (2, 8) `shouldBe` ["9", "0123456789"]
      del (0, 1) (2, 8) `shouldBe` ["09", "0123456789"]

      del (0, 0) (1, 8) `shouldBe` ["9", "0123456789", "0123456789"]
      del (0, 1) (1, 8) `shouldBe` ["09", "0123456789", "0123456789"]

      del (0, 1) (0, 8) `shouldBe` ["09", "0123456789", "0123456789", "0123456789"]
      del (1, 1) (1, 8) `shouldBe` ["0123456789", "09", "0123456789", "0123456789"]
      del (2, 1) (2, 8) `shouldBe` ["0123456789", "0123456789", "09", "0123456789"]
      del (3, 1) (3, 8) `shouldBe` ["0123456789", "0123456789", "0123456789", "09"]

      del (0, 0) (3, 9) `shouldBe` [""]

      del (0, 0) (0, 10) `shouldBe` ["0123456789", "0123456789", "0123456789"]
      del (0, 0) (1, 10) `shouldBe` ["0123456789", "0123456789"]
      del (0, 0) (2, 10) `shouldBe` ["0123456789"]
      del (0, 0) (3, 10) `shouldBe` [""]
