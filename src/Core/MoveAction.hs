module Core.MoveAction
  ( MoveAction(..)
  , moveTop
  , moveBottom
  , moveLeft
  , moveRight
  , moveAt
  , applyMoveAction
  )
where

import Core.Cursor

data MoveAction = MTop Int
                | MBottom Int
                | MLeft Int
                | MRight Int
                | MAt Row Col


moveTop :: Int -> MoveAction
moveTop =
  MTop


moveBottom :: Int -> MoveAction
moveBottom =
  MBottom


moveLeft :: Int -> MoveAction
moveLeft =
  MLeft


moveRight :: Int -> MoveAction
moveRight =
  MRight


moveAt :: Row -> Col -> MoveAction
moveAt =
  MAt


applyMoveAction :: MoveAction -> (Row, Col) -> (Row, Col)
applyMoveAction action (row, col) =
  case action of
    MTop times ->
      (row - times, col)

    MBottom times ->
      (row + times, col)

    MLeft times ->
      (row, col - times)

    MRight times ->
      (row, col + times)

    MAt mrow mcol ->
      (mrow, mcol)
