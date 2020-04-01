module Core.App.Data
  ( App
  , empty
  , getWindow
  , close
  , isAppClosed
  , getContent
  , getBuffer
  , modifyWindow
  , setMode
  , getMode
  , getConsole
  , modifyConsole
  )
where

import qualified Core.Buffer as Buffer
import Core.Buffer (Buffer)

import qualified Core.Window as Window
import Core.Window (Window)

import qualified Core.Mode as Mode
import Core.Mode (Mode)

import qualified Core.Console as Console
import Core.Console (Console)

data App =
  App
  { appWindow :: Window
  , appClose :: Bool
  , appMode :: Mode
  , appConsole :: Console
  }
  deriving (Show)


empty :: App
empty =
  App
  { appWindow = Window.empty
  , appClose = False
  , appMode = Mode.Normal
  , appConsole = Console.empty
  }


getContent :: App -> [String]
getContent =
  Window.getContent . getWindow


getBuffer :: App -> Buffer
getBuffer =
  Window.getBuffer . getWindow


getWindow :: App -> Window
getWindow =
  appWindow


getConsole :: App -> Console
getConsole =
  appConsole


isAppClosed :: App -> Bool
isAppClosed =
  appClose


close :: App -> App
close app =
  app { appClose = True }


modifyWindow :: (Window -> Window) -> App -> App
modifyWindow f app =
  app { appWindow = f . getWindow $ app }


modifyConsole :: (Console -> Console) -> App -> App
modifyConsole f app =
  app { appConsole = f . getConsole $ app }


setMode :: Mode -> App -> App
setMode mode app =
  app {appMode = mode}


getMode :: App -> Mode
getMode =
  appMode
