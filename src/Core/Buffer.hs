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
  case action of
    MTop times ->
      buffer
        { bufCursor = mkCursor (cropRow (row - times)) col }
    MBottom times ->
      buffer
        { bufCursor = mkCursor (cropRow (row + times)) col }
    MLeft times ->
      buffer
        { bufCursor = mkCursor row (cropCol (col - times)) }

    MRight times ->
      buffer
        { bufCursor = mkCursor row (cropCol (col + times)) }
    MAt mrow mcol ->
      buffer
        { bufCursor = mkCursor mrow mcol }

  where
    cursor :: Cursor
    cursor = bufCursor buffer

    row :: Row
    row = getRow cursor

    col :: Col
    col = getCol cursor

    currentRowsLen :: Int
    currentRowsLen = length (bufContent buffer) - 1

    currentColsLen :: Int
    currentColsLen = length (bufContent buffer !! row) - 1

    cropRow :: Row -> Row
    cropRow = crop 0 currentRowsLen

    cropCol :: Col -> Col
    cropCol = crop 0 currentColsLen


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
