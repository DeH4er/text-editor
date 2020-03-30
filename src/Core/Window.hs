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


getAllCursors :: Window -> [Cursor]
getAllCursors window =
  winMainCursor window : winCursors window


getMainCursor :: Window -> Cursor
getMainCursor =
  winMainCursor


empty :: Window
empty =
  Window
  { winBuffer = Buffer.empty
  , winRect = Rect.empty
  , winMainCursor = Cursor.empty
  , winCursors = [Cursor.new 1 0, Cursor.new 2 0]
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


modifyCursors :: ([Cursor] -> [Cursor]) -> Window -> Window
modifyCursors f window =
  window
  { winCursors = tail newWinCursors
  , winMainCursor = head newWinCursors
  }
    where
      newWinCursors = f . getAllCursors $ window


moveCursors :: MoveAction -> Window -> Window
moveCursors action window = withNewRect
    where
      withNewRect :: Window
      withNewRect = modifyRect (doMoveView $ getMainCursor withNewCursors) withNewCursors

      withNewCursors :: Window
      withNewCursors = modifyCursors doMoveCursors window

      doMoveCursors :: [Cursor] -> [Cursor]
      doMoveCursors cursors =
        moveCursor action content <$> cursors

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

      content :: [String]
      content = Buffer.getContent . getBuffer $ window


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

-- moveCursor :: MoveAction -> Window -> Window
-- moveCursor action window =
--   window { winCursors = newCursors }
--     where
--       newCursors = undefined
--         -- filterSame . fmap (cropCursor . mkCursor . applyMoveAction action . getRowCol) $ winCursors window
--         -- [cursor]
--
--       cursor = mkCursor croppedRow croppedCol
--
--       croppedCol =
--         if croppedRowLength == 0
--           then
--             0
--           else
--             crop 0 croppedRowLength newCol
--
--       croppedRow =
--         if rowsLength == 0
--           then
--             0
--           else
--             crop 0 (rowsLength - 1) newRow
--
--       rowsLength =
--         length (bufContent window)
--
--       croppedRowLength =
--         length (bufContent window !! croppedRow)
--
--       (newRow, newCol) =
--         applyMoveAction action (row, col)
--
--       (row, col) =
--         getRowCol $ bufCursor window
