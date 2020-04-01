module Core.Mode
  ( Mode(..)
  , Action(..)
  )
where

import Core.Event
import Core.MoveAction


data Mode
  = Normal
  | Insert
  | Command
  deriving (Show, Eq)


data Action
  = MoveCursors MoveAction
  | SetMode Mode
  | InsertChar Char
  | InsertCharConsole Char
  | DeleteCharConsole
  | ExecuteConsole
  | BreakLine
  | DeleteChar
  | OpenFile FilePath
  | SaveFile
  | SaveFileAs FilePath
  | Quit
  deriving (Show, Eq)
