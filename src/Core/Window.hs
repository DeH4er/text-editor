module Core.Window
  ( moveCursors
  , insertChar
  , breakLine
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


breakLine :: Window -> Window
breakLine =
  fitViewByMainCursor . withMovedCursors . withBreakedLines
    where
      withBreakedLines :: Window -> Window
      withBreakedLines =
        modifyContentByReversedGroupedCursors doBreak

      withMovedCursors :: Window -> Window
      withMovedCursors =
        modifyGroupedCursors doMove

      doBreak :: [[Cursor]] -> [String] -> [String]
      doBreak [] content = content
      doBreak (rowCursors : others) content =
        breakRow rowCursors $ doBreak others content

      breakRow :: [Cursor] -> [String] -> [String]
      breakRow [] content = content
      breakRow (cursor:cursors) content =
        breakDownAt row col $ breakRow cursors content
          where
            (row, col) = Cursor.getRowCol cursor

      doMove :: [[Cursor]] -> [[Cursor]]
      doMove =
        mapIndex moveAtRow

      moveAtRow :: Int -> [Cursor] -> [Cursor]
      moveAtRow i =
        mapIndex $ moveSingleCursor i

      moveSingleCursor :: Int -> Int -> Cursor -> Cursor
      moveSingleCursor i j =
        Cursor.modify doModify
          where
            doModify :: (Cursor.Row, Cursor.Col) -> (Cursor.Row, Cursor.Col)
            doModify (row, col) =
              (row + i + j + 1, 0)


modifyGroupedCursors :: ([[Cursor]] -> [[Cursor]]) -> Window -> Window
modifyGroupedCursors f =
  modifyCursors $ concat . f . groupCursors


modifyContentByGroupedCursors :: ([[Cursor]] -> [String] -> [String]) -> Window -> Window
modifyContentByGroupedCursors f window =
  modifyContent (f $ getGroupedCursors window) window


modifyContentByReversedGroupedCursors :: ([[Cursor]] -> [String] -> [String]) -> Window -> Window
modifyContentByReversedGroupedCursors f window =
  modifyContent (f $ getGroupedCursors window) window


fitViewByMainCursor :: Window -> Window
fitViewByMainCursor window =
  fitViewByCursor (getMainCursor window) window


fitViewByCursor :: Cursor -> Window -> Window
fitViewByCursor =
  modifyRect . doMoveView
    where
      doMoveView :: Cursor -> Rect -> Rect
      doMoveView cursor rect =
        Rect.translate newRow newCol rect
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


getGroupedCursorsReversed :: Window -> [[Cursor]]
getGroupedCursorsReversed =
  groupCursorsReversed . getAllCursors


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


groupCursorsReversed :: [Cursor] -> [[Cursor]]
groupCursorsReversed =
  reverse . fmap reverse . groupCursors


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


breakDownAt :: Cursor.Row -> Cursor.Col -> [String] -> [String]
breakDownAt _ _ [] =
  []

breakDownAt 0 col (x:xs) =
  take col x : drop col x : xs

breakDownAt row col (x:xs) =
  x : breakDownAt (row - 1) col xs


