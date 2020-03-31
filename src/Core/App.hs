module Core.App
  ( App
  , initApp
  , handle
  , handleIO
  , getLines
  , getBuffer
  , isAppClosed
  , getWindow
  , onResize
  )
where


import Core.Event
import Core.Fs
import qualified Core.Buffer as Buffer

import qualified Core.Window as Window
import Core.Window (Window)

import Core.MoveAction

data App =
  App
  { appBuffer :: Buffer.Buffer
  , appWindow :: Window
  , appClose :: Bool
  }
  deriving (Show)


initApp :: App
initApp =
  App
  { appBuffer = Buffer.empty
  , appWindow = Window.empty
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
      case Buffer.getFilepath (getBuffer app) of
        Just filepath ->
          saveFile fsService filepath (unlines $ getLines app)

        Nothing ->
          return . return $ ()

      return app

    EvKey evKey ->
      case evKey of
        KChar '\t' ->
          let win1 = Window.insertChar ' ' $ getWindow app
              win2 = Window.insertChar ' ' win1
           in return $ app { appWindow = win2 }

        KChar c ->
          return $ app { appWindow = Window.insertChar c $ getWindow app }

        KUp ->
          return $ app { appWindow = Window.moveCursors (moveTop 1) (getWindow app) }

        KDown ->
          return $ app { appWindow = Window.moveCursors (moveBottom 1) (getWindow app) }

        KLeft ->
          return $ app { appWindow = Window.moveCursors (moveLeft 1) (getWindow app) }

        KRight ->
          return $ app { appWindow = Window.moveCursors (moveRight 1) (getWindow app) }

        KEnter ->
          return $ app { appWindow = Window.breakLine (getWindow app) }

        KBackspace ->
          return $ app { appBuffer = Buffer.deleteChar (getBuffer app) }

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
handleIO = handle ioFsService


getLines :: App -> [String]
getLines app =
  Buffer.getContent . Window.getBuffer . getWindow $ app


getBuffer :: App -> Buffer.Buffer
getBuffer =
  appBuffer


getWindow :: App -> Window
getWindow =
  appWindow


onResize :: (Int, Int) -> App -> App
onResize (width, height) app =
  app { appWindow = Window.resize width height $ getWindow app }


isAppClosed :: App -> Bool
isAppClosed =
  appClose
