module Core.Window
  ( moveCursors
  , insertChar
  , breakLine
  , deleteChar
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
        modifyContentByGroupedCursors doBreak

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


deleteChar :: Window -> Window
deleteChar =
  fitViewByMainCursor . withMovedCursors . withDeletedChars
    where
      withMovedCursors :: Window -> Window
      withMovedCursors =
        modifyGroupedCursorsByContent doMove

      withDeletedChars :: Window -> Window
      withDeletedChars =
        modifyContentByGroupedCursors doDelete

      doDelete :: [[Cursor]] -> [String] -> [String]
      doDelete [] content = content
      doDelete (rowCursors : others) content =
        deleteCharRow rowCursors $ doDelete others content

      deleteCharRow :: [Cursor] -> [String] -> [String]
      deleteCharRow [] content = content
      deleteCharRow (cursor:cursors) content =
        joinDeleteUp cursor $ deleteCharRow cursors content

      doMove :: [String] -> [[Cursor]] -> [[Cursor]]
      doMove content = mapCollect 0 (moveRowCursors content)

      moveRowCursors :: [String] -> Int -> [Cursor] -> ([Cursor], Int)
      moveRowCursors content zeroColCount all@(firstCursor:cursors)
        | firstCursorRow == 0 =
          if firstCursorCol == 0
            then
              (firstCursor : moveCursorsLeft content cursors, zeroColCount)
            else
              (moveCursorsLeft content all, zeroColCount)
        | firstCursorCol == 0 =
          (moveCursorTopRightmost zeroColCount content firstCursor : moveCursorsLeft content cursors, zeroColCount + 1)
        | otherwise =
          (moveCursorsLeft content all, zeroColCount)
        where
          (firstCursorRow, firstCursorCol) = Cursor.getRowCol firstCursor

      moveCursorsLeft :: [String] -> [Cursor] -> [Cursor]
      moveCursorsLeft content =
        mapIndex (moveCursorLeft content)

      moveCursorLeft :: [String] -> Int -> Cursor -> Cursor
      moveCursorLeft content times =
        Cursor.modify (\(row, col) -> cropCursor content (row, col - times - 1))

      moveCursorTopRightmost :: Int -> [String] -> Cursor -> Cursor
      moveCursorTopRightmost zeroColCount content cursor =
        Cursor.new croppedRow croppedCol
          where
            (croppedRow, croppedCol) = cropCursor content (newRow, newCol)
            newRow = row - zeroColCount - 1
            newCol = length $ content !! (row - zeroColCount - 1)
            row = Cursor.getRow cursor


modifyGroupedCursors :: ([[Cursor]] -> [[Cursor]]) -> Window -> Window
modifyGroupedCursors f =
  modifyCursors $ concat . f . groupCursors


modifyGroupedCursorsByContent :: ([String] -> [[Cursor]] -> [[Cursor]]) -> Window -> Window
modifyGroupedCursorsByContent f window =
  modifyCursors (concat . f (getContent window) . groupCursors) window


modifyContentByGroupedCursors :: ([[Cursor]] -> [String] -> [String]) -> Window -> Window
modifyContentByGroupedCursors f window =
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


breakDownAt :: Cursor.Row -> Cursor.Col -> [String] -> [String]
breakDownAt _ _ [] =
  []

breakDownAt 0 col (x:xs) =
  take col x : drop col x : xs

breakDownAt row col (x:xs) =
  x : breakDownAt (row - 1) col xs


joinDeleteUp :: Cursor -> [String] -> [String]
joinDeleteUp cursor content =
  if col <= 0
    then
      joinLinesUp row content
    else
      modifyAt row deleteCharLine content
  where
    deleteCharLine :: String -> String
    deleteCharLine =
      removeAt $ col - 1

    (row, col) = Cursor.getRowCol cursor


joinLinesUp :: Cursor.Row -> [String] -> [String]
joinLinesUp _ [] =
  []

joinLinesUp _ [x1] =
  [x1]

joinLinesUp 1 (x1:x2:xs) =
  x1 <> x2 : xs

joinLinesUp row (x:xs) =
  x : joinLinesUp (row - 1) xs
