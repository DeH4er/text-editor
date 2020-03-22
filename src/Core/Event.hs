module Core.Event
  ( closeEvent
  , keyEvent
  , keyEsc
  , keyChar
  , openEvent
  , Event(..)
  , Key(..)
  )
where


data Event = EvClose
           | EvKey Key
           | EvOpen FilePath
           | EvSave


closeEvent :: Event
closeEvent =
  EvClose


keyEvent :: Char -> Event
keyEvent c =
  EvKey $ keyChar c


openEvent :: FilePath -> Event
openEvent = EvOpen


data Key = KEsc
         | KChar Char


keyEsc =
  KEsc


keyChar =
  KChar


