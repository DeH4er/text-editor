module Ui where

import Graphics.Vty                  as Vty
import qualified Core
import qualified Core.Utils as Utils

ui :: Vty -> Core.App -> IO ()
ui vty app = if Core.isAppClosed app
  then return ()
  else do
    let pic = makePicture app
    update vty pic
    e <- nextEvent vty
    case mapEvent e of
      Just mappedEvent -> do
        newApp <- Core.handle mappedEvent app
        ui vty newApp
      Nothing -> ui vty app


mapEvent :: Event -> Maybe Core.Event
mapEvent (EvKey (KChar c) _) = Just . Core.keyEvent $ Core.keyChar c
mapEvent (EvKey KUp _) = Just . Core.keyEvent $ Core.keyUp
mapEvent (EvKey KDown _) = Just . Core.keyEvent $ Core.keyDown
mapEvent (EvKey KLeft _) = Just . Core.keyEvent $ Core.keyLeft
mapEvent (EvKey KRight _) = Just . Core.keyEvent $ Core.keyRight
mapEvent (EvKey KEnter _) = Just . Core.keyEvent $ Core.keyEnter
mapEvent (EvKey KEsc _) = Just Core.closeEvent
mapEvent _                           = Nothing


makePicture :: Core.App -> Picture
makePicture app = picForImage $ makeImage cursor lines
 where
  lines = Core.getLines app
  buffer = Core.getBuffer app
  cursor = Core.getCursor buffer
  -- col = Core.getCol cursor
  -- row = Core.getRow cursor
  -- attr = defAttr -- `withForeColor` green
  -- cursorAttr = defAttr `withForeColor` black `withBackColor` white
  -- mkImage = string attr
  -- images = mkImage <$> lines
  -- image = vertCat images
  -- pic = picForImage image


makeImage :: Core.Cursor -> [String] -> Image
makeImage cursor lines = image
  where
    col = Core.getCol cursor
    row = Core.getRow cursor
    attr = defAttr -- `withForeColor` green
    cursorAttr = defAttr `withForeColor` black `withBackColor` white
  -- mkImage = string attr
  -- images = mkImage <$> lines
  -- image = vertCat images

    image :: Image
    image = lineImage row col lines


lineImage :: Core.Row -> Core.Col -> [String] -> Image
lineImage 0 col (x:xs) = lineCursorImage col x <-> lineImage (-1) col xs
lineImage _ _ [] = emptyImage
lineImage row col (x:xs) = string attr x <-> lineImage (row - 1) col xs

lineCursorImage :: Core.Col -> String -> Image
lineCursorImage col x = before <|> cursor <|> after
  where
    before = string attr (take col x)
    cursor = char cursorAttr (x !! col)
    after = string attr (drop (col + 1) x)

attr = defAttr -- `withForeColor` green
cursorAttr = defAttr `withForeColor` black `withBackColor` white


startUi :: Core.App -> IO ()
startUi app = do
  cfg <- standardIOConfig
  vty <- mkVty cfg
  ui vty app
  shutdown vty


