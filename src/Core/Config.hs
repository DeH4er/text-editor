module Core.Config
  ( getBinding
  , getCommand
  , CommandState (..)
  )
where

import Core.Mode
import Core.Event
import Core.Movement
import Core.Deletion

import Data.Maybe (fromMaybe, listToMaybe)
import Data.List (find)


type Binding = (Mode, [Key], Action)


bindings :: [Binding]
bindings =
  [ (Normal, [KChar 'h'], MoveCursors MLeft)
  , (Normal, [KChar 'l'], MoveCursors MRight)
  , (Normal, [KChar 'k'], MoveCursors MTop)
  , (Normal, [KChar 'j'], MoveCursors MBottom)

  , (Normal, [KChar 'w'], MoveCursors MForwardWord)
  , (Normal, [KChar 'e'], MoveCursors MForwardEndWord)
  , (Normal, [KChar 'b'], MoveCursors MBackwardWord)

  , (Normal, [KChar '$'], MoveCursors MEndLine)
  , (Normal, [KChar '0'], MoveCursors MStartLine)

  , (Normal, [KChar 'g', KChar 'g'  ], MoveCursors MStartContent)
  , (Normal, [KChar 'G'], MoveCursors MEndContent)

  , (Normal, [KChar 'D'], MoveCursors MForwardHalfScreen)
  , (Normal, [KChar 'U'], MoveCursors MBackwardHalfScreen)

  , (Normal, [KChar 'i'], SetMode Insert)
  , (Normal, [KChar ';'], SetMode Command)
  , (Normal, [KChar 'm'], MarkPhantom)
  , (Normal, [KChar 's'], CreatePhantoms)
  , (Normal, [KEsc], RemoveCursors)

  , (Normal, [KChar 'd', KChar 'w'], Delete MForwardWord)

  , (Insert, [KChar 'j', KChar 'k'], SetMode Normal)
  , (Insert, [KEsc], SetMode Normal)
  , (Insert, [KBackspace], DeleteChar)
  , (Insert, [KEnter], BreakLine)

  , (Command,[KEsc],  SetMode Normal)
  , (Command,[KEnter],  ExecuteConsole)
  , (Command,[KBackspace],  DeleteCharConsole)
  ]


data CommandState =
  KeepGoing
  | None
  | Found Action
  deriving (Show, Eq)


getBinding :: Mode -> [Key] -> CommandState
getBinding mode keys =
  fromMaybe (fromMaybe None $ find byKeepGoing states) $ find byFound states
    where
      states :: [CommandState]
      states =
        checkBinding keys <$> filter byMode bindings

      checkBinding :: [Key] -> Binding -> CommandState
      checkBinding [] (_, [], action) = Found action
      checkBinding [] (_, (x:xs), _) = KeepGoing
      checkBinding (y:ys) (mode, (x:xs), action) =
        if x == y
          then
            checkBinding ys (mode, xs, action)
          else
            None

      byFound :: CommandState -> Bool
      byFound (Found _) = True
      byFound _ = False

      byKeepGoing :: CommandState -> Bool
      byKeepGoing KeepGoing = True
      byKeepGoing _ = False

      byMode :: Binding -> Bool
      byMode (item, _, _) =
        item == mode


-- TODO: add parser for command
getCommand :: String -> Maybe Action
getCommand "" = Nothing
getCommand "q" = Just Quit
getCommand "s" = Just SaveFile
getCommand cmd =
  case cmd of
    ('o' : ' ' : filepath) ->
      Just . OpenFile $ filepath

    ('s' : ' ' : filepath) ->
      Just . SaveFileAs $ filepath

    _ ->
      Nothing


