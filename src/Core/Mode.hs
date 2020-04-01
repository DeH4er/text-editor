module Core.Mode
  ( Mode(..)
  , Action(..)
  , getAction
  )
where

import Core.Event
import Core.MoveAction


data Mode
  = NormalMode
  | InsertMode
  | CommandMode
  deriving (Show, Eq)


data Action
  = MoveCursors MoveAction
  | SetMode Mode
  | InsertChar Char
  | BreakLine
  | DeleteChar
  | OpenFile FilePath
  | SaveFile
  | SaveFileAs FilePath
  | Quit
  deriving (Show, Eq)


getAction :: [(Key, Mode, Action)] -> Key -> Mode -> Maybe Action
getAction [] _ _ =
  Nothing

getAction ((sKey, sMode, action):xs) key mode
  | key == sKey && mode == sMode =
    Just action

  | otherwise =
    getAction xs key mode
