module Core.App
  ( handle
  , handleIO
  , onResize
  , interpretAction
  , interpretActionIO
  , module Core.App.Data
  )
where


import qualified Core.Buffer as Buffer
import Core.Buffer (Buffer)

import qualified Core.Window as Window
import Core.Window (Window)

import qualified Core.Console as Console
import Core.Console (Console)

import qualified Core.Config as Config

import Core.Mode
import Core.Event
import Core.Fs
import Core.App.Data


handle :: Monad m => FsService m -> Key -> App -> m App
handle fsService key app =
  case Config.getBinding (getMode app1) (getCommandAcc app1) of
    Config.Found action ->
      interpretAction fsService action app2

    Config.KeepGoing ->
      return app1

    Config.None ->
      defaultHandle fsService key app2
  where
    app1 = pushCommandAcc key app
    app2 = clearCommandAcc app1


defaultHandle :: Monad m => FsService m -> Key -> App -> m App
defaultHandle fsService (KChar c) app =
  case getMode app of
    Insert ->
      interpretAction fsService (InsertChar c) app
    Command ->
      interpretAction fsService (InsertCharConsole c) app
    _ ->
      return app

defaultHandle _ _ app =
  return app


handleIO :: Key -> App -> IO App
handleIO =
  handle ioFsService


interpretActionIO :: Action -> App -> IO App
interpretActionIO =
  interpretAction ioFsService


interpretAction :: Monad m => FsService m -> Action -> App -> m App


interpretAction _ (MoveCursors movement) app =
  return . modifyWindow (Window.moveCursors movement) $ app


interpretAction _ (InsertChar char) app =
  return . modifyWindow (Window.insertChar char) $ app


interpretAction _ BreakLine app =
  return . modifyWindow Window.breakLine $ app


interpretAction _ DeleteChar app =
  return . modifyWindow Window.deleteChar $ app


interpretAction _ Quit app =
  return . close $ app


interpretAction _ (SetMode mode) app =
  return $ setMode mode app


interpretAction fsService (OpenFile path) app = do
  eitherContent <- loadFile fsService path

  case eitherContent of
    Left _ ->
      return app

    Right content ->
      return . modifyWindow (const $ Window.fromBuffer buffer) $ app
      where
        buffer = Buffer.fromContent path (lines content)


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


interpretAction _ (InsertCharConsole c) app =
  return . modifyConsole (Console.insertChar c) $ app


interpretAction _ DeleteCharConsole app =
  return . modifyConsole Console.deleteChar $ app


interpretAction fsService ExecuteConsole app =
  case Config.getCommand consoleContent of
    Just action -> do
      app1 <- interpretAction fsService action app
      exitConsole app1
    Nothing ->
      exitConsole app
    where
      exitConsole app = do
        app1 <- interpretAction fsService (SetMode Normal) app
        return . modifyConsole Console.clearContent $ app1

      consoleContent = Console.getContent . getConsole $ app


interpretAction _ MarkPhantom app =
  return . modifyWindow Window.markPhantom $ app


interpretAction _ CreatePhantoms app =
  return . modifyWindow Window.createPhantoms $ app


interpretAction _ RemoveCursors app =
  return . modifyWindow Window.removeCursors $ app


onResize :: (Int, Int) -> App -> App
onResize (width, height) =
  modifyWindow $ Window.resize width height
