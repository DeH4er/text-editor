module Core.App
  ( App
  , initApp
  , handle
  , getLines
  , getBuffer
  , isAppClosed
  )
where


import Core.Event
import Core.Fs
import Core.Buffer


data App = App
  { appBuffer :: Buffer
  , appClose :: Bool
  }


initApp :: App
initApp =
  App
  { appBuffer = emptyBuffer
  , appClose = False
  }


handle
  :: FsService m
  => Event
  -> App
  -> m App
handle event app =
  case event of
    EvClose ->
      return $ app { appClose = True }

    EvSave -> do
      case getFilepath (getBuffer app) of
        Just filepath ->
          saveFile filepath (unlines $ getLines app)

        Nothing ->
          return . return $ ()

      return app

    EvKey evKey ->
      case evKey of
        KChar c ->
          return $ app { appBuffer = insertChar (getBuffer app) c }

        KUp ->
          return $ app { appBuffer = moveCursor (moveTop 1) (getBuffer app) }

        KDown ->
          return $ app { appBuffer = moveCursor (moveBottom 1) (getBuffer app) }

        KLeft ->
          return $ app { appBuffer = moveCursor (moveLeft 1) (getBuffer app) }

        KRight ->
          return $ app { appBuffer = moveCursor (moveRight 1) (getBuffer app) }

        KEnter ->
          return $ app { appBuffer = breakLine (getBuffer app) }

        KBackspace ->
          return $ app { appBuffer = deleteChar (getBuffer app) }

    EvOpen filepath -> do
      eitherContent <- loadFile filepath

      case eitherContent of
        Left _ ->
          return app

        Right content ->
          return $ app { appBuffer = loadContent (getBuffer app) filepath (lines content) }


getLines :: App -> [String]
getLines app =
  getContent $ appBuffer app


getBuffer :: App -> Buffer
getBuffer =
  appBuffer


isAppClosed :: App -> Bool
isAppClosed =
  appClose
