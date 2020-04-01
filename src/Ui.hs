module Ui where

import Control.Monad (unless)
import Graphics.Vty
import qualified Core

import qualified Core.Window as Window
import Core.Window (Window)

import qualified Core.Mode as Mode
import Core.Mode (Mode)

import qualified Core.Console as Console
import Core.Console (Console)

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
    update vty (makePicture bounds app)
    e <- nextEvent vty
    case mapEvent e of
      Just mappedEvent -> do
        newApp <- Core.handleIO mappedEvent app
        ui vty newApp

      Nothing ->
        ui vty app


mapEvent :: Event -> Maybe Core.Key

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


makePicture :: DisplayRegion -> Core.App -> Picture
makePicture region app =
  Picture
  { picCursor = NoCursor
  , picLayers = getLayers region app
  , picBackground = background
  }


background :: Background
background =
  Background
  { backgroundChar = ' '
  , backgroundAttr = attr
  }


getLayers :: DisplayRegion -> Core.App -> [Image]
getLayers region app =
  case Core.getMode app of
    Mode.Command ->
      getCommandModeLayers region (Core.getConsole app) <> baseLayers
    _ ->
      baseLayers

  where
    baseLayers = cropImage app <$> cursorLayers app <> [textLayer app]


getCommandModeLayers :: DisplayRegion -> Console -> [Image]
getCommandModeLayers (width, height) console =
  [cursor, text, input, bg]
    where
      cursor =
        translate cursorPosition 2 $ char commandCursorAttr cursorChar
          where
            cursorPosition =
              2 + Console.getPosition console

            cursorChar =
              ' '

      text =
        translate 2 2 $ string commandTextAttr $ Console.getContent console

      input =
        translate 2 2 $ charFill commandInputAttr ' ' inputWidth 1
          where inputWidth = width - 4

      bg =
        translate 1 1 $charFill commandBgAttr ' ' bgWidth 3
          where bgWidth = width - 2


textLayer :: Core.App -> Image
textLayer app =
  mconcat $ string attr <$> Core.getContent app


cursorLayers :: Core.App -> [Image]
cursorLayers app =
  cursors <> phantomCursors
  where
    cursors =
      doImage cursorAttr <$> (Window.getAllCursors . Core.getWindow $ app)

    phantomCursors =
      doImage phantomCursorAttr <$> (Window.getPhantoms . Core.getWindow $ app)

    doImage :: Attr -> Cursor.Cursor -> Image
    doImage attr cursor = image
      where
        (row, col) =
          Cursor.getRowCol cursor

        cursorChar =
          getCursorChar row col $ Core.getContent app

        image =
          translate col row $ char attr cursorChar


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


commandBgAttr :: Attr
commandBgAttr =
  defAttr `withBackColor` gray1


commandCursorAttr :: Attr
commandCursorAttr =
  defAttr `withForeColor` black `withBackColor` white


commandTextAttr :: Attr
commandTextAttr =
  defAttr `withForeColor` white `withBackColor` black


commandInputAttr :: Attr
commandInputAttr =
  defAttr `withForeColor` white `withBackColor` black


cursorAttr :: Attr
cursorAttr =
  defAttr `withForeColor` black `withBackColor` white


phantomCursorAttr :: Attr
phantomCursorAttr =
  defAttr `withForeColor` white `withBackColor` gray1


gray1 :: Color
gray1 =
  rgbColor 0x33 0x33 0x33
