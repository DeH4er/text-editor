module Core.App
  ( App
  , initApp
  , handle
  , handleIO
  , getContent
  , isAppClosed
  , getWindow
  , onResize
  )
where


import Core.Event
import Core.Fs

import qualified Core.Buffer as Buffer
import Core.Buffer (Buffer)

import qualified Core.Window as Window
import Core.Window (Window)

import Core.MoveAction

data App =
  App
  { appWindow :: Window
  , appClose :: Bool
  }
  deriving (Show)


initApp :: App
initApp =
  App
  { appWindow = Window.empty
  , appClose = False
  }


handle
  :: Monad m
  => FsService m
  -> Event
  -> App
  -> m App
handle fsService event app =
  case event of
    EvClose ->
      return $ app { appClose = True }

    EvSave -> do
      case Window.getFilepath . getWindow $ app of
        Just filepath ->
          saveFile fsService filepath (unlines . getContent $ app)

        Nothing ->
          return . return $ ()

      return app

    EvKey evKey ->
      case evKey of
        KChar '\t' ->
          return . modifyWindow (Window.insertChar ' ' . Window.insertChar ' ') $ app

        KChar c ->
          return . modifyWindow (Window.insertChar c) $ app

        KUp ->
          return . modifyWindow (Window.moveCursors $ moveTop 1) $ app

        KDown ->
          return . modifyWindow (Window.moveCursors $ moveBottom 1) $ app

        KLeft ->
          return . modifyWindow (Window.moveCursors $ moveLeft 1) $ app

        KRight ->
          return . modifyWindow (Window.moveCursors $ moveRight 1) $ app

        KEnter ->
          return . modifyWindow Window.breakLine $ app

        KBackspace ->
          return . modifyWindow Window.deleteChar $ app

    EvOpen filepath -> do
      eitherContent <- loadFile fsService filepath

      case eitherContent of
        Left _ ->
          return app

        Right content ->
          let buffer = Buffer.loadContent (getBuffer app) filepath (lines content)
            in return $ app { appWindow = Window.loadBuffer buffer $ getWindow app}


handleIO
  :: Event
  -> App
  -> IO App
handleIO =
  handle ioFsService


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
