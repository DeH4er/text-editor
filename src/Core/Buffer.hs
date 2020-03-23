module Core.Buffer
  ( Buffer
  , MoveAction
  , emptyBuffer
  , moveCursor
  , loadContent
  , insertChar
  , getContent
  , moveTop
  , moveBottom
  , moveLeft
  , moveRight
  , moveAt
  , breakLine
  , getCursor
  )
where

import Core.Cursor
import Core.Utils


data MoveAction = MTop Int
                | MBottom Int
                | MLeft Int
                | MRight Int
                | MAt Row Col


getCursor :: Buffer -> Cursor
getCursor =
  bufCursor


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


data Buffer = Buffer
  { bufFilepath :: Maybe FilePath
  , bufCursor :: Cursor
  , bufContent :: [String]
  }


getContent :: Buffer -> [String]
getContent = bufContent

emptyBuffer :: Buffer
emptyBuffer =
  Buffer
  { bufFilepath = Nothing
  , bufCursor = mkCursor 0 0
  , bufContent = [""]
  }


loadContent :: Buffer -> FilePath -> [String] -> Buffer
loadContent buffer filepath content =
  buffer
  { bufFilepath = Just filepath
  , bufContent = content
  }


moveCursor :: MoveAction -> Buffer -> Buffer
moveCursor action buffer =
  buffer { bufCursor = mkCursor croppedRow croppedCol }
    where
      cursor :: Cursor
      cursor = bufCursor buffer

      row :: Row
      row = getRow cursor

      col :: Col
      col = getCol cursor

      (newRow, newCol) = getNewCursorPosition action (row, col)

      rowsLength = length (bufContent buffer) - 1
      croppedRow = crop 0 rowsLength newRow

      croppedRowLength = length (bufContent buffer !! croppedRow) - 1
      croppedCol = crop 0 croppedRowLength newCol

getNewCursorPosition :: MoveAction -> (Row, Col) -> (Row, Col)
getNewCursorPosition action (row, col) =
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


insertChar :: Buffer -> Char -> Buffer
insertChar buffer char = newBuffer

  where
    cursor :: Cursor
    cursor = bufCursor buffer

    content :: [String]
    content = bufContent buffer

    row :: Row
    row = getRow cursor

    col :: Col
    col = getCol cursor

    newCursor :: Cursor
    newCursor = mkCursor row (col + 1)

    newContent :: [String]
    newContent = modifyAt row (insertAt col char) content

    newBuffer :: Buffer
    newBuffer = buffer { bufCursor = newCursor
                       , bufContent = newContent }


breakLine :: Buffer -> Buffer
breakLine buffer = newBuffer
  where
    cursor :: Cursor
    cursor = bufCursor buffer

    col :: Col
    col = getCol cursor

    row :: Row
    row = getRow cursor

    content :: [String]
    content = bufContent buffer

    newCursor :: Cursor
    newCursor = mkCursor (row + 1) 0

    newContent :: [String]
    newContent = breakDownAt row col content

    newBuffer :: Buffer
    newBuffer = buffer { bufCursor = newCursor, bufContent = newContent}


breakDownAt :: Row -> Col -> [[a]] -> [[a]]
breakDownAt row col [] = []
breakDownAt 0 col (x:xs) = take col x : drop col x : xs
breakDownAt row col (x:xs) = x : breakDownAt (row - 1) col xs
