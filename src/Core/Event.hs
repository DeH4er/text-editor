module Core.Event
  ( Event(..)
  , Key(..)
  , closeEvent
  , saveEvent
  , keyEvent
  , keyEsc
  , keyChar
  , openEvent
  , keyEnter
  , keyUp
  , keyDown
  , keyLeft
  , keyRight
  , keyBackspace
  )
where


data Event = EvClose
           | EvKey Key
           | EvOpen FilePath
           | EvSave


saveEvent :: Event
saveEvent =
  EvSave


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
         | KBackspace

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


keyBackspace =
  KBackspace
