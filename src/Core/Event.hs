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


keyEsc :: Key
keyEsc =
  KEsc


keyChar :: Char -> Key
keyChar =
  KChar


keyUp :: Key
keyUp =
  KUp


keyDown :: Key
keyDown =
  KDown


keyLeft :: Key
keyLeft =
  KLeft


keyRight :: Key
keyRight =
  KRight


keyEnter :: Key
keyEnter =
  KEnter


keyBackspace :: Key
keyBackspace =
  KBackspace


