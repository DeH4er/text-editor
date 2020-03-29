module Core.Window
  ( Window
  , empty
  , getBuffer
  , getRect
  , getAllCursors
  , loadBuffer
  , setRect
  , resize
  , resizeWidth
  , resizeHeight
  , translate
  , translateRow
  , translateCol
  )
where


import qualified Core.Window.Rect as Rect
import qualified Core.Buffer as Buffer
import qualified Core.Cursor as Cursor
import Core.MoveAction


data Window
  = Window
    { winBuffer :: Buffer.Buffer
    , winRect :: Rect.Rect
    , winMainCursor :: Cursor.Cursor
    , winCursors :: [Cursor.Cursor]
    }
    deriving (Show, Eq)


getRect :: Window -> Rect.Rect
getRect =
  winRect


getBuffer :: Window -> Buffer.Buffer
getBuffer =
  winBuffer


getAllCursors :: Window -> [Cursor.Cursor]
getAllCursors window =
  winMainCursor window : winCursors window


empty :: Window
empty =
  Window
  { winBuffer = Buffer.empty
  , winRect = Rect.empty
  , winMainCursor = Cursor.empty
  , winCursors = []
  }


loadBuffer :: Buffer.Buffer -> Window -> Window
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


modifyRect :: (Rect.Rect -> Rect.Rect) -> Window -> Window
modifyRect f window =
  window {winRect = f . getRect $ window}

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
