module Core.Window
  ( moveCursors
  , insertChar
  , module Core.Window.Data
  )
where

import Data.List (groupBy, sortBy, sortOn)

import qualified Core.Window.Rect as Rect
import Core.Window.Rect (Rect)

import qualified Core.Buffer as Buffer
import Core.Buffer (Buffer)

import qualified Core.Cursor as Cursor
import Core.Cursor (Cursor)

import Core.MoveAction
import Core.Utils

import Core.Window.Data


moveCursors :: MoveAction -> Window -> Window
moveCursors action =
  fitViewByMainCursor . modifyCursorsByContent (doMoveCursors action)
    where
      doMoveCursors :: MoveAction -> [String] -> [Cursor] -> [Cursor]
      doMoveCursors action content cursors =
        moveCursor action content <$> cursors


insertChar :: Char -> Window -> Window
insertChar char =
  fitViewByMainCursor . withMovedCursors . withInsertedChar
    where
      withMovedCursors :: Window -> Window
      withMovedCursors =
        modifyGroupedCursors moveGroupedCursors

      withInsertedChar :: Window -> Window
      withInsertedChar =
        modifyContentByCursors insertAtCursors

      insertAtCursors :: [Cursor] -> [String] -> [String]
      insertAtCursors [] content =
        content

      insertAtCursors (cursor : cursors) content =
        insertAtCursor cursor $ insertAtCursors cursors content

      insertAtCursor :: Cursor -> [String] -> [String]
      insertAtCursor cursor =
        modifyAt row $ insertAt col char
          where
            (row, col) =
              Cursor.getRowCol cursor

      moveGroupedCursors :: [[Cursor]] -> [[Cursor]]
      moveGroupedCursors =
        fmap $ mapIndex doMapIndex

      doMapIndex :: Int -> Cursor -> Cursor
      doMapIndex i =
        Cursor.modify $ doModify i

      doModify :: Int -> (Cursor.Row, Cursor.Col) -> (Cursor.Row, Cursor.Col)
      doModify i (row, col) =
        (row, col + i + 1)


modifyGroupedCursors :: ([[Cursor]] -> [[Cursor]]) -> Window -> Window
modifyGroupedCursors f =
  modifyCursors $ concat . f . groupCursors


fitViewByMainCursor :: Window -> Window
fitViewByMainCursor window =
  fitViewByCursor (getMainCursor window) window


fitViewByCursor :: Cursor -> Window -> Window
fitViewByCursor =
  modifyRect . doMoveView
    where
      doMoveView :: Cursor -> Rect -> Rect
      doMoveView cursor rect =
        Rect.new newRow newCol rWidth rHeight
          where
            newRow
              | row >= (rRow + rHeight - 1) =
                row - rHeight + 1
              | row < rRow =
                row
              | otherwise =
                rRow

            newCol
              | col >= (rCol + rWidth - 1) =
                col - rWidth + 1
              | col < rCol =
                col
              | otherwise =
                rCol

            rRow =
              Rect.getRow rect

            rCol =
              Rect.getCol rect

            rWidth =
              Rect.getWidth rect

            rHeight =
              Rect.getHeight rect

            (row, col) =
              Cursor.getRowCol cursor


getGroupedCursors :: Window -> [[Cursor]]
getGroupedCursors =
  groupCursors . getAllCursors


groupCursors :: [Cursor] -> [[Cursor]]
groupCursors =
  sortGroupedCursors . doGroupCursors
    where
      sortGroupedCursors :: [[Cursor]] -> [[Cursor]]
      sortGroupedCursors [] =
        []

      sortGroupedCursors (rowCursors : others) =
        sortOn Cursor.getCol rowCursors : sortGroupedCursors others

      doGroupCursors :: [Cursor] -> [[Cursor]]
      doGroupCursors =
        groupBy byRows
          where
            byRows :: Cursor -> Cursor -> Bool
            byRows cursor1 cursor2 =
              Cursor.getRow cursor1 == Cursor.getRow cursor2


moveCursor :: MoveAction -> [String] -> Cursor -> Cursor
moveCursor action content cursor =
  cropped
    where
      cropped =
        cropCursor content `Cursor.modify` moved

      moved =
        applyMoveAction action `Cursor.modify` cursor


cropCursor :: [String] -> (Cursor.Row, Cursor.Col) -> (Cursor.Row, Cursor.Col)
cropCursor content (row, col) =
  (newRow, newCol)
    where
      newRow =
        crop 0 (length content - 1) row

      newCol =
        crop 0 (length $ content !! newRow) col
