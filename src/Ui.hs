module Ui where

import Control.Monad (unless)
import Graphics.Vty
import qualified Core


ui :: Vty -> Core.App -> IO ()
ui vty app =
  unless (Core.isAppClosed app) $ do
    update vty (makePicture app)
    e <- nextEvent vty
    case mapEvent e of
      Just mappedEvent -> do
        newApp <- Core.handle mappedEvent app
        ui vty newApp

      Nothing ->
        ui vty app


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


makeImage :: Core.Cursor -> [String] -> Image
makeImage cursor lines = linesImage row col lines
  where
    row :: Core.Row
    row = Core.getRow cursor

    col :: Core.Col
    col = Core.getCol cursor


linesImage :: Core.Row -> Core.Col -> [String] -> Image
linesImage 0 col (x:xs) = lineCursorImage col x <-> linesImage (-1) col xs
linesImage _ _ [] = emptyImage
linesImage row col (x:xs) = string attr x <-> linesImage (row - 1) col xs


lineCursorImage :: Core.Col -> String -> Image
lineCursorImage col x =
  case x of
    [] ->
      char cursorAttr ' '
    _ ->
      before <|> cursor <|> after
  where
    before = string attr (take col x)
    cursor = char cursorAttr (x !! col)
    after = string attr (drop (col + 1) x)


attr :: Attr
attr = defAttr


cursorAttr :: Attr
cursorAttr = defAttr `withForeColor` black `withBackColor` white


startUi :: Core.App -> IO ()
startUi app = do
  cfg <- standardIOConfig
  vty <- mkVty cfg
  ui vty app
  shutdown vty
