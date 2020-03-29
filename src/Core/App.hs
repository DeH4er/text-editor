module Core.App
  ( App
  , initApp
  , handle
  , handleIO
  , getLines
  , getBuffer
  , isAppClosed
  )
where


import Core.Event
import Core.Fs
import qualified Core.Buffer as Buffer
import Core.MoveAction


data App =
  App
  { appBuffer :: Buffer.Buffer
  , appClose :: Bool
  }


initApp :: App
initApp =
  App
  { appBuffer = Buffer.empty
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
          let buf1 = Buffer.insertChar (getBuffer app) ' '
              buf2 = Buffer.insertChar buf1 ' '
           in return $ app { appBuffer = buf2 }

        KChar c ->
          return $ app { appBuffer = Buffer.insertChar (getBuffer app) c }

        KUp ->
          return $ app { appBuffer = Buffer.moveCursor (moveTop 1) (getBuffer app) }

        KDown ->
          return $ app { appBuffer = Buffer.moveCursor (moveBottom 1) (getBuffer app) }

        KLeft ->
          return $ app { appBuffer = Buffer.moveCursor (moveLeft 1) (getBuffer app) }

        KRight ->
          return $ app { appBuffer = Buffer.moveCursor (moveRight 1) (getBuffer app) }

        KEnter ->
          return $ app { appBuffer = Buffer.breakLine (getBuffer app) }

        KBackspace ->
          return $ app { appBuffer = Buffer.deleteChar (getBuffer app) }

    EvOpen filepath -> do
      eitherContent <- loadFile fsService filepath

      case eitherContent of
        Left _ ->
          return app

        Right content ->
          return $ app { appBuffer = Buffer.loadContent (getBuffer app) filepath (lines content) }


handleIO
  :: Event
  -> App
  -> IO App
handleIO = handle ioFsService


getLines :: App -> [String]
getLines app =
  Buffer.getContent $ appBuffer app


getBuffer :: App -> Buffer.Buffer
getBuffer =
  appBuffer


isAppClosed :: App -> Bool
isAppClosed =
  appClose
