module Core.Config
  ( getBinding
  , getCommand
  )
where

import Core.Mode
import Core.Event
import Core.MoveAction

getBinding :: Key -> Mode -> Maybe Action
getBinding (KChar 'h') Normal = Just . MoveCursors $ moveLeft 1
getBinding (KChar 'l') Normal = Just . MoveCursors $ moveRight 1
getBinding (KChar 'k') Normal = Just . MoveCursors $ moveTop 1
getBinding (KChar 'j') Normal = Just . MoveCursors $ moveBottom 1
getBinding (KChar 'i') Normal = Just . SetMode $ Insert
getBinding (KChar ';') Normal = Just . SetMode $ Command
getBinding KEsc        Normal = Just Quit

getBinding KEsc        Insert = Just . SetMode $ Normal
getBinding (KChar c)   Insert = Just . InsertChar $ c
getBinding KBackspace  Insert = Just DeleteChar
getBinding KEnter      Insert = Just BreakLine

getBinding KEsc        Command = Just . SetMode $ Normal
getBinding (KChar c)   Command = Just . InsertCharConsole $ c
getBinding KEnter      Command = Just ExecuteConsole
getBinding KBackspace  Command = Just DeleteCharConsole

getBinding _ _ = Nothing


-- TODO: add parser for command
getCommand :: String -> Maybe Action
getCommand "q" = Just Quit
getCommand "quit" = Just Quit
getCommand "s" = Just SaveFile
getCommand "" = Nothing
getCommand cmd =
  case cmd of
    ('o' : ' ' : filepath) ->
      Just . OpenFile $ filepath

    ('s' : ' ' : filepath) ->
      Just . SaveFileAs $ filepath

    _ ->
      Nothing