module Core.Config
  ( getBinding
  , getCommand
  )
where

import Core.Mode
import Core.Event
import Core.Movement

getBinding :: Key -> Mode -> Maybe Action
getBinding (KChar 'h') Normal = Just . MoveCursors $ MLeft
getBinding (KChar 'l') Normal = Just . MoveCursors $ MRight
getBinding (KChar 'k') Normal = Just . MoveCursors $ MTop
getBinding (KChar 'j') Normal = Just . MoveCursors $ MBottom
getBinding (KChar 'w') Normal = Just . MoveCursors $ MForwardWord
getBinding (KChar 'e') Normal = Just . MoveCursors $ MForwardEndWord
getBinding (KChar 'b') Normal = Just . MoveCursors $ MBackwardWord
getBinding (KChar '$') Normal = Just . MoveCursors $ MEndLine
getBinding (KChar '0') Normal = Just . MoveCursors $ MStartLine
getBinding (KChar 'i') Normal = Just . SetMode $ Insert
getBinding (KChar ';') Normal = Just . SetMode $ Command
getBinding (KChar 'm') Normal = Just MarkPhantom
getBinding (KChar 's') Normal = Just CreatePhantoms
getBinding KEsc        Normal = Just RemoveCursors

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
