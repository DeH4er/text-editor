module Core.Config
  ( bindings
  )
where

import Core.Mode
import Core.Event
import Core.MoveAction

bindings :: [(Key, Mode, Action)]
bindings =
  [ (KChar 'h', NormalMode, MoveCursors $ moveLeft 1)
  , (KChar 'l', NormalMode, MoveCursors $ moveRight 1)
  , (KChar 'j', NormalMode, MoveCursors $ moveBottom 1)
  , (KChar 'k', NormalMode, MoveCursors $ moveTop 1)
  , (KChar 'i', NormalMode, SetMode InsertMode)
  , (KBackspace, InsertMode, DeleteChar)
  , (KEnter, InsertMode, BreakLine)
  , (KEsc, InsertMode, SetMode NormalMode)
  , (KEsc, NormalMode, Quit)
  ]
