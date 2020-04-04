module Core.Mode
  ( Mode(..)
  , Action(..)
  )
where

import Core.Event
import Core.Movement


data Mode
  = Normal
  | Insert
  | Command
  deriving (Show, Eq)


data Action
  = MoveCursors Movement
  | SetMode Mode
  | InsertChar Char
  | InsertCharConsole Char
  | DeleteCharConsole
  | RemoveCursors
  | ExecuteConsole
  | BreakLine
  | DeleteChar
  | OpenFile FilePath
  | SaveFile
  | SaveFileAs FilePath
  | MarkPhantom
  | CreatePhantoms
  | Quit
  deriving (Show, Eq)
