module Core.Window.Data
  ( Window
  , getBuffer
  , getRect
  , getContent
  , getFilepath
  , getAllCursors
  , getMainCursor
  , empty
  , loadBuffer
  , modifyRect
  , modifyBuffer
  , modifyCursors
  , modifyContent
  , modifyContentByCursors
  , modifyCursorsByContent
  , resize
  , setRect
  )
where


import qualified Core.Buffer as Buffer
import Core.Buffer (Buffer)

import qualified Core.Cursor as Cursor
import Core.Cursor (Cursor)

import qualified Core.Window.Rect as Rect
import Core.Window.Rect (Rect)


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


getFilepath :: Window -> Maybe FilePath
getFilepath =
  Buffer.getFilepath . getBuffer

getContent :: Window -> [String]
getContent =
  Buffer.getContent . getBuffer


getAllCursors :: Window -> [Cursor]
getAllCursors window =
  winMainCursor window : winCursors window


getMainCursor :: Window -> Cursor
getMainCursor =
  winMainCursor


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


modifyContent :: ([String] -> [String]) -> Window -> Window
modifyContent f =
  modifyBuffer $ Buffer.modifyContent f


modifyContentByCursors :: ([Cursor] -> [String] -> [String]) -> Window -> Window
modifyContentByCursors f window =
  modifyContent (f $ getAllCursors window) window


modifyCursorsByContent :: ([String] -> [Cursor] -> [Cursor]) -> Window -> Window
modifyCursorsByContent f window =
  modifyCursors (f $ getContent window) window


empty :: Window
empty =
  Window
  { winBuffer = Buffer.empty
  , winRect = Rect.empty
  , winMainCursor = Cursor.empty
  , winCursors = []
  }


loadBuffer :: Buffer -> Window -> Window
loadBuffer buffer =
  (modifyBuffer . const $ buffer)


resize :: Rect.Width -> Rect.Height  -> Window -> Window
resize width height =
  modifyRect $ Rect.resize width height


setRect :: Rect -> Window -> Window
setRect rect =
  modifyRect $ const rect
