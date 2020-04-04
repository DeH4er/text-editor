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
  | MoveForwardWord
  | MoveForwardEndWord
  | MoveEndLine
  | MoveStartLine
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
