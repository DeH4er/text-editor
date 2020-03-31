module Core.App
  ( App
  , initApp
  , handle
  , handleIO
  , getContent
  , isAppClosed
  , getWindow
  , onResize
  , interpretAction
  , interpretActionIO
  )
where


import Core.Event
import Core.Fs

import qualified Core.Buffer as Buffer
import Core.Buffer (Buffer)

import qualified Core.Window as Window
import Core.Window (Window)

import Core.MoveAction
import Core.Key
import qualified Core.Config as Config

data App =
  App
  { appWindow :: Window
  , appClose :: Bool
  , appMode :: Mode
  }
  deriving (Show)


initApp :: App
initApp =
  App
  { appWindow = Window.empty
  , appClose = False
  , appMode = NormalMode
  }


handle :: Monad m => FsService m -> Event -> App -> m App
handle fsService event app =
  case event of
    EvKey evKey ->
      case getAction Config.bindings evKey (appMode app) of
        Just action ->
          interpretAction fsService action app

        Nothing ->
          if appMode app == InsertMode
            then
              case evKey of
                KChar '\t' ->
                  return . modifyWindow (Window.insertChar ' ' . Window.insertChar ' ') $ app

                KChar c ->
                  return . modifyWindow (Window.insertChar c) $ app

                _ ->
                  return app
            else
              return app
    _ ->
      return app


handleIO :: Event -> App -> IO App
handleIO =
  handle ioFsService


interpretActionIO :: Action -> App -> IO App
interpretActionIO =
  interpretAction ioFsService


interpretAction :: Monad m => FsService m -> Action -> App -> m App

interpretAction _ (MoveCursors action) app =
  return . modifyWindow (Window.moveCursors action) $ app


interpretAction _ (InsertChar char) app =
  return . modifyWindow (Window.insertChar char) $ app


interpretAction _ BreakLine app =
  return . modifyWindow Window.breakLine $ app


interpretAction _ DeleteChar app =
  return . modifyWindow Window.deleteChar $ app


interpretAction _ Quit app =
  return $ app { appClose = True }

interpretAction _ (SetMode mode) app =
  return $ app { appMode = mode }

interpretAction fsService (OpenFile path) app = do
  eitherContent <- loadFile fsService path

  case eitherContent of
    Left _ ->
      return app

    Right content ->
      return . modifyWindow (Window.loadBuffer buffer) $ app
      where
        buffer = Buffer.loadContent (getBuffer app) path (lines content)

interpretAction fsService SaveFile app = do
  case Window.getFilepath . getWindow $ app of
    Just filepath ->
      saveFile fsService filepath (unlines . getContent $ app)

    Nothing ->
      return . return $ ()

  return app

interpretAction fsService (SaveFileAs filepath) app = do
  saveFile fsService filepath (unlines . getContent $ app)
  return app


getContent :: App -> [String]
getContent =
  Window.getContent . getWindow


getBuffer :: App -> Buffer
getBuffer =
  Window.getBuffer . getWindow


getWindow :: App -> Window
getWindow =
  appWindow


onResize :: (Int, Int) -> App -> App
onResize (width, height) app =
  app { appWindow = Window.resize width height $ getWindow app }


isAppClosed :: App -> Bool
isAppClosed =
  appClose


modifyWindow :: (Window -> Window) -> App -> App
modifyWindow f app =
  app { appWindow = f . getWindow $ app }
