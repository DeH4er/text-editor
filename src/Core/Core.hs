module Core.Core
  ( App
  , Cursor
  , getRow
  , getCol
  , initApp
  , handle
  , isAppClosed
  , getLines
  , getBuffer
  )
where


import Core.Event
import Core.Fs
import Core.Cursor
import Core.Buffer


data App = App
  { appBuffer :: Buffer
  , appClose :: Bool
  }


initApp :: App
initApp = App
        { appBuffer = emptyBuffer
        , appClose = False
        }


handle :: Event -> App -> IO App
handle event app =
  case event of
    EvClose ->
      return $ app { appClose = True }

    EvSave -> do
      case getFilepath (getBuffer app) of
        Just filepath ->
          writeFile filepath (unlines $ getLines app)

        Nothing ->
          return ()

      return app

    EvKey evKey ->
      case evKey of
        KChar c ->
          return $ app { appBuffer = insertChar (appBuffer app) c }

        KUp ->
          return $ app { appBuffer = moveCursor (moveTop 1) (appBuffer app) }

        KDown ->
          return $ app { appBuffer = moveCursor (moveBottom 1) (appBuffer app) }

        KLeft ->
          return $ app { appBuffer = moveCursor (moveLeft 1) (appBuffer app) }

        KRight ->
          return $ app { appBuffer = moveCursor (moveRight 1) (appBuffer app) }

        KEnter ->
          return $ app { appBuffer = breakLine (appBuffer app) }

        KBackspace ->
          return $ app { appBuffer = deleteChar (appBuffer app) }

    EvOpen filepath -> do
      eitherContent <- loadFile filepath

      case eitherContent of
        Left _ ->
          return app

        Right content ->
          return $ app { appBuffer = loadContent buffer filepath (lines content) }
            where
              buffer :: Buffer
              buffer = appBuffer app



getLines :: App -> [String]
getLines app =
  getContent $ appBuffer app


getBuffer :: App -> Buffer
getBuffer =
  appBuffer


isAppClosed :: App -> Bool
isAppClosed =
  appClose
