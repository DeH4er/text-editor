module Core.Window
  ( Window
  , empty
  , getBuffer
  , getRect
  , getAllCursors
  , getMainCursor
  , loadBuffer
  , setRect
  , resize
  , resizeWidth
  , resizeHeight
  , translate
  , translateRow
  , translateCol
  , moveCursors
  , insertChar
  )
where


import qualified Core.Window.Rect as Rect
import Core.Window.Rect (Rect)

import qualified Core.Buffer as Buffer
import Core.Buffer (Buffer)

import qualified Core.Cursor as Cursor
import Core.Cursor (Cursor)

import Core.MoveAction
import Core.Utils

import Data.List (groupBy, sortBy, sortOn)


data Window
  = Window
    { winBuffer :: Buffer
    , winRect :: Rect
    , winMainCursor :: Cursor
    , winCursors :: [Cursor]
    }
    deriving (Show, Eq)


getRect :: Window -> Rect
getRect =
  winRect


getBuffer :: Window -> Buffer
getBuffer =
  winBuffer


getContent :: Window -> [String]
getContent =
  Buffer.getContent . getBuffer


getAllCursors :: Window -> [Cursor]
getAllCursors window =
  winMainCursor window : winCursors window


modifyContent :: ([String] -> [String]) -> Window -> Window
modifyContent f =
  modifyBuffer $ Buffer.modifyContent f


modifyContentByCursors :: ([Cursor] -> [String] -> [String]) -> Window -> Window
modifyContentByCursors f window =
  modifyContent (f $ getAllCursors window) window


modifyGroupedCursors :: ([[Cursor]] -> [[Cursor]]) -> Window -> Window
modifyGroupedCursors f =
  modifyCursors $ concat . f . groupCursors


modifyCursorsByContent :: ([String] -> [Cursor] -> [Cursor]) -> Window -> Window
modifyCursorsByContent f window =
  modifyCursors (f $ getContent window) window


fitViewByMainCursor :: Window -> Window
fitViewByMainCursor window =
  fitViewByCursor (getMainCursor window) window


fitViewByCursor :: Cursor -> Window -> Window
fitViewByCursor = modifyRect . doMoveView
  where
    doMoveView :: Cursor -> Rect -> Rect
    doMoveView cursor rect = Rect.new newRow newCol rWidth rHeight
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

        rRow = Rect.getRow rect
        rCol = Rect.getCol rect
        rWidth = Rect.getWidth rect
        rHeight = Rect.getHeight rect
        (row, col) = Cursor.getRowCol cursor


getGroupedCursors :: Window -> [[Cursor]]
getGroupedCursors = groupCursors . getAllCursors


groupCursors :: [Cursor] -> [[Cursor]]
groupCursors = sortGroupedCursors . doGroupCursors
  where
    sortGroupedCursors :: [[Cursor]] -> [[Cursor]]
    sortGroupedCursors [] = []
    sortGroupedCursors (rowCursors : others) =
      sortOn Cursor.getCol rowCursors : sortGroupedCursors others

    doGroupCursors :: [Cursor] -> [[Cursor]]
    doGroupCursors =
      groupBy byRows
        where
          byRows :: Cursor -> Cursor -> Bool
          byRows cursor1 cursor2 = Cursor.getRow cursor1 == Cursor.getRow cursor2


getMainCursor :: Window -> Cursor
getMainCursor =
  winMainCursor


empty :: Window
empty =
  Window
  { winBuffer = Buffer.empty
  , winRect = Rect.empty
  , winMainCursor = Cursor.empty
  , winCursors = [Cursor.new 1 0, Cursor.new 1 5, Cursor.new 2 0]
  }


loadBuffer :: Buffer -> Window -> Window
loadBuffer buffer window =
  window {winBuffer = buffer}


setRect :: Cursor.Row -> Cursor.Col -> Rect.Width -> Rect.Height -> Window -> Window
setRect row col width height =
  modifyRect $ const (Rect.new row col width height)


resize :: Rect.Width -> Rect.Height -> Window -> Window
resize width height =
  modifyRect $ Rect.resize width height


resizeWidth :: Rect.Width -> Window -> Window
resizeWidth width =
  modifyRect $ Rect.resizeWidth width


resizeHeight :: Rect.Height -> Window -> Window
resizeHeight height =
  modifyRect $ Rect.resizeHeight height


translate :: Cursor.Row -> Cursor.Col -> Window -> Window
translate row col =
  modifyRect $ Rect.translate row col


translateRow :: Cursor.Row -> Window -> Window
translateRow row =
  modifyRect $ Rect.translateRow row


translateCol :: Cursor.Col -> Window -> Window
translateCol col =
  modifyRect $ Rect.translateCol col


modifyRect :: (Rect -> Rect) -> Window -> Window
modifyRect f window =
  window {winRect = f . getRect $ window}


modifyBuffer :: (Buffer -> Buffer) -> Window -> Window
modifyBuffer f window =
  window {winBuffer = f . getBuffer $ window}


modifyCursors :: ([Cursor] -> [Cursor]) -> Window -> Window
modifyCursors f window =
  window
  { winCursors = tail newWinCursors
  , winMainCursor = head newWinCursors
  }
    where
      newWinCursors = f . getAllCursors $ window


moveCursors :: MoveAction -> Window -> Window
moveCursors action =
  fitViewByMainCursor . modifyCursorsByContent doMoveCursors
    where
      doMoveCursors :: [String] -> [Cursor] -> [Cursor]
      doMoveCursors content cursors =
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
      insertAtCursors [] content = content
      insertAtCursors (cursor : cursors) content =
        insertAtCursor cursor $ insertAtCursors cursors content

      insertAtCursor :: Cursor -> [String] -> [String]
      insertAtCursor cursor =
        modifyAt row $ insertAt col char
          where
            (row, col) = Cursor.getRowCol cursor

      moveGroupedCursors :: [[Cursor]] -> [[Cursor]]
      moveGroupedCursors = fmap $ mapIndex doMapIndex

      doMapIndex :: Int -> Cursor -> Cursor
      doMapIndex i = Cursor.modify $ doModify i

      doModify :: Int -> (Cursor.Row, Cursor.Col) -> (Cursor.Row, Cursor.Col)
      doModify i (row, col) = (row, col + i + 1)


moveCursor :: MoveAction -> [String] -> Cursor -> Cursor
moveCursor action content cursor = cropped
    where
      cropped = cropCursor content `Cursor.modify` moved
      moved = applyMoveAction action `Cursor.modify` cursor


cropCursor :: [String] -> (Cursor.Row, Cursor.Col) -> (Cursor.Row, Cursor.Col)
cropCursor content (row, col) = (newRow, newCol)
  where
    newRow = crop 0 (length content - 1) row
    newCol = crop 0 (length $ content !! newRow) col
