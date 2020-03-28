module Ui where

import Control.Monad (unless)
import Graphics.Vty
import qualified Core
import qualified Core.Utils as Utils


startUi :: Core.App -> IO ()
startUi app = do
  cfg <- standardIOConfig
  vty <- mkVty cfg
  ui vty app
  shutdown vty


ui :: Vty -> Core.App -> IO ()
ui vty app =
  unless (Core.isAppClosed app) $ do
    bounds <- displayBounds (outputIface vty)
    update vty (makePicture app bounds)
    e <- nextEvent vty
    case mapEvent e of
      Just mappedEvent -> do
        newApp <- Core.handleIO mappedEvent app
        ui vty newApp

      Nothing ->
        ui vty app


mapEvent :: Event -> Maybe Core.Event
mapEvent (EvKey (KChar s) [MCtrl]) =
  Just Core.saveEvent

mapEvent (EvKey (KChar c) _) =
  Just . Core.keyEvent $ Core.keyChar c

mapEvent (EvKey KUp _) =
  Just . Core.keyEvent $ Core.keyUp

mapEvent (EvKey KDown _) =
  Just . Core.keyEvent $ Core.keyDown

mapEvent (EvKey KLeft _) =
  Just . Core.keyEvent $ Core.keyLeft

mapEvent (EvKey KRight _) =
  Just . Core.keyEvent $ Core.keyRight

mapEvent (EvKey KEnter _) =
  Just . Core.keyEvent $ Core.keyEnter

mapEvent (EvKey KBS _) =
  Just . Core.keyEvent $ Core.keyBackspace

mapEvent (EvKey KEsc _) =
  Just Core.closeEvent

mapEvent _ =
  Nothing


makePicture :: Core.App -> DisplayRegion -> Picture
makePicture app region =
  Picture
  { picCursor = NoCursor
  , picLayers = getLayers app region
  , picBackground = background
  }


background :: Background
background =
  Background
  { backgroundChar = ' '
  , backgroundAttr = attr
  }


getLayers :: Core.App -> DisplayRegion -> [Image]
getLayers app region =
  cropImage app region <$> [cursorLayer app, textLayer app]


textLayer :: Core.App -> Image
textLayer app =
  mconcat $ string attr <$> Core.getLines app


cursorLayer :: Core.App -> Image
cursorLayer app  = image
  where
    (row, col) =
      Core.getRowCol . Core.getCursor . Core.getBuffer $ app

    cursorChar =
      getCursorChar row col $ Core.getLines app

    image =
      translate col row $ char cursorAttr cursorChar


cropImage :: Core.App -> DisplayRegion -> Image -> Image
cropImage app (width, height) image = croppedImage
  where
    (row, col) =
      Core.getRowCol . Core.getCursor . Core.getBuffer $ app

    croppedXImage =
      if col >= width then
        translateX (width - col - 1) image
      else
        image

    croppedImage =
      if row >= height then
        translateY (height - row - 1) croppedXImage
      else
        croppedXImage


getCursorChar :: Core.Row -> Core.Col -> [String] -> Char
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

