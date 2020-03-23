module Core.Event
  ( Event(..)
  , Key(..)
  , closeEvent
  , keyEvent
  , keyEsc
  , keyChar
  , openEvent
  , keyEnter
  , keyUp
  , keyDown
  , keyLeft
  , keyRight
  )
where


data Event = EvClose
           | EvKey Key
           | EvOpen FilePath
           | EvSave


closeEvent :: Event
closeEvent =
  EvClose


openEvent :: FilePath -> Event
openEvent = EvOpen


keyEvent :: Key -> Event
keyEvent =
  EvKey


data Key = KEsc
         | KChar Char
         | KUp
         | KDown
         | KLeft
         | KRight
         | KEnter

keyEsc =
  KEsc


keyChar =
  KChar


keyUp =
  KUp


keyDown =
  KDown


keyLeft =
  KLeft


keyRight =
  KRight


keyEnter =
  KEnter
