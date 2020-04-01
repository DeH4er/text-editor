module Ui where

import Control.Monad (unless)
import Graphics.Vty
import qualified Core

import qualified Core.Window as Window
import Core.Window (Window)

import qualified Core.Window.Rect as Rect

import qualified Core.Cursor as Cursor
import qualified Core.Utils as Utils


startUi :: Core.App -> IO ()
startUi app = do
  cfg <- standardIOConfig
  vty <- mkVty cfg
  ui vty app
  shutdown vty


ui :: Vty -> Core.App -> IO ()
ui vty _app =
  unless (Core.isAppClosed _app) $ do
    bounds <- displayBounds (outputIface vty)
    let app = Core.onResize bounds _app
    update vty (makePicture app)
    e <- nextEvent vty
    case mapEvent e of
      Just mappedEvent -> do
        newApp <- Core.handleIO mappedEvent app
        ui vty newApp

      Nothing ->
        ui vty app


mapEvent :: Event -> Maybe Core.Key
-- mapEvent (EvKey (KChar s) [MCtrl]) =
--   Just Core.saveEvent

mapEvent (EvKey (KChar c) _) =
  Just . Core.keyChar $ c

mapEvent (EvKey KUp _) =
  Just Core.keyUp

mapEvent (EvKey KDown _) =
  Just Core.keyDown

mapEvent (EvKey KLeft _) =
  Just Core.keyLeft

mapEvent (EvKey KRight _) =
  Just Core.keyRight

mapEvent (EvKey KEnter _) =
  Just Core.keyEnter

mapEvent (EvKey KBS _) =
  Just Core.keyBackspace

mapEvent (EvKey KEsc _) =
  Just Core.keyEsc

mapEvent _ =
  Nothing


makePicture :: Core.App -> Picture
makePicture app =
  Picture
  { picCursor = NoCursor
  , picLayers = getLayers app
  , picBackground = background
  }


background :: Background
background =
  Background
  { backgroundChar = ' '
  , backgroundAttr = attr
  }


getLayers :: Core.App -> [Image]
getLayers app =
  cropImage app <$> cursorLayers app <> [textLayer app]


textLayer :: Core.App -> Image
textLayer app =
  mconcat $ string attr <$> Core.getContent app


cursorLayers :: Core.App -> [Image]
cursorLayers app = doImage <$> (Window.getAllCursors . Core.getWindow $ app)
  where
    doImage :: Cursor.Cursor -> Image
    doImage cursor = image
      where
        (row, col) =
          Cursor.getRowCol cursor

        cursorChar =
          getCursorChar row col $ Core.getContent app

        image =
          translate col row $ char cursorAttr cursorChar


cropImage :: Core.App -> Image -> Image
cropImage app image = cropped
  where
    cropped = crop rWidth rHeight translated
    translated = translate (-rCol) (-rRow) image

    window = Core.getWindow app
    rect = Window.getRect window

    rRow = Rect.getRow rect
    rCol = Rect.getCol rect
    rWidth = Rect.getWidth rect
    rHeight = Rect.getHeight rect


getCursorChar :: Cursor.Row -> Cursor.Col -> [String] -> Char
getCursorChar _ _ [] =
  ' '

getCursorChar 0 col (x:xs) =
  if col == length x
     then
      ' '
     else
      x !! col

getCursorChar row col (x:xs) =
  getCursorChar (row - 1) col xs


attr :: Attr
attr =
  defAttr


cursorAttr :: Attr
cursorAttr =
  defAttr `withForeColor` black `withBackColor` white

