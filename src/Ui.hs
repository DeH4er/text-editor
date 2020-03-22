module Ui where

import qualified Graphics.Vty                  as Vty
import           Core

ui :: Vty.Vty -> App -> IO ()
ui vty app = if isAppClosed app
  then return ()
  else do
    let pic = makePicture app
    Vty.update vty pic
    e <- Vty.nextEvent vty
    case mapEvent e of
      Just mappedEvent -> do
        newApp <- handle mappedEvent app
        ui vty newApp
      Nothing -> ui vty app


mapEvent :: Vty.Event -> Maybe Event
mapEvent (Vty.EvKey (Vty.KChar c) _) = Just $ keyEvent c
mapEvent (Vty.EvKey Vty.KEsc _) = Just closeEvent
mapEvent _                           = Nothing


makePicture :: App -> Vty.Picture
makePicture app = pic
 where
  lines   = getLines app
  attr    = Vty.defAttr -- `withForeColor` green
  mkImage = Vty.string attr
  images  = mkImage <$> lines
  image   = Vty.vertCat images
  pic     = Vty.picForImage image


startUi :: App -> IO ()
startUi app = do
  cfg <- Vty.standardIOConfig
  vty <- Vty.mkVty cfg
  ui vty app
  Vty.shutdown vty


