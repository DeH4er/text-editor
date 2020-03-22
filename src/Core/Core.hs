module Core.Core
  ( App
  , initApp
  , handle
  , isAppClosed
  , getLines

  , Event
  , Key
  , closeEvent
  , keyEvent
  , keyEsc
  , keyChar
  , openEvent
  )
where


import Core.Event
import Core.Fs


data App = App
  { appLines :: [String]
  , appClose :: Bool
  }


initApp :: App
initApp =
  App { appLines = [""], appClose = False }


handle :: Event -> App -> IO App
handle event app =
  case event of
    EvClose ->
      return $ app { appClose = True }

    EvKey (KChar c) ->
      return $ app { appLines = newLines }
        where
          lines = appLines app
          firstLine = head lines
          updatedFirstLine = firstLine <> [c]
          newLines = updatedFirstLine : tail lines

    EvOpen filepath -> do
      eitherContent <- loadFile filepath

      case eitherContent of
        Left _ ->
          return app

        Right content ->
          return $ app { appLines = lines content }



getLines :: App -> [String]
getLines =
  appLines


isAppClosed :: App -> Bool
isAppClosed =
  appClose
