module Core.Buffer
  ( Buffer
  , MoveAction
  , getContent
  , getFilepath
  , emptyBuffer
  , moveCursor
  , loadContent
  , insertChar
  , moveTop
  , moveBottom
  , moveLeft
  , moveRight
  , moveAt
  , breakLine
  , getCursor
  , deleteChar
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
getContent =
  bufContent


getFilepath :: Buffer -> Maybe FilePath
getFilepath =
  bufFilepath


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
      croppedCol =
        if croppedRowLength == 0
          then
            0
          else
            crop 0 croppedRowLength newCol

      croppedRow =
        if rowsLength == 0
          then
            0
          else
            crop 0 (rowsLength - 1) newRow

      rowsLength =
        length (bufContent buffer)

      croppedRowLength =
        length (bufContent buffer !! croppedRow)

      (newRow, newCol) =
        applyMoveAction action (row, col)

      (row, col) =
        getRowCol $ bufCursor buffer


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


insertChar :: Buffer -> Char -> Buffer
insertChar buffer char = newBuffer
  where
    newBuffer :: Buffer
    newBuffer =
      buffer { bufCursor = newCursor , bufContent = newContent }

    newCursor :: Cursor
    newCursor =
      mkCursor row (col + 1)

    newContent :: [String]
    newContent =
      modifyAt row (insertAt col char) content

    content :: [String]
    content =
      bufContent buffer

    (row, col) =
      getRowCol $ bufCursor buffer

deleteChar :: Buffer -> Buffer
deleteChar buffer = newBuffer
  where
    newBuffer :: Buffer
    newBuffer =
      buffer { bufCursor = newCursor, bufContent = newContent}

    newCursor :: Cursor
    newCursor =
      if col <= 0
        then
          let newRow = cropMin 0 (row - 1)
              newCol =
                if row == 0
                  then
                    0
                  else
                    length (content !! newRow)
           in mkCursor newRow newCol
        else
          mkCursor row (cropMin 0 (col - 1))

    newContent :: [String]
    newContent =
      if col <= 0
        then
          joinLinesUp row content
        else
          modifyAt row deleteCharLine content

    deleteCharLine :: String -> String
    deleteCharLine =
      removeAt $ col - 1

    content :: [String]
    content =
      bufContent buffer

    (row, col) =
      getRowCol $ bufCursor buffer

breakLine :: Buffer -> Buffer
breakLine buffer = newBuffer
  where
    newBuffer :: Buffer
    newBuffer =
      buffer { bufCursor = newCursor, bufContent = newContent}

    newCursor :: Cursor
    newCursor =
      mkCursor (row + 1) 0

    newContent :: [String]
    newContent =
      breakDownAt row col content

    content :: [String]
    content =
      bufContent buffer

    (row, col) =
      getRowCol $ bufCursor buffer


breakDownAt :: Row -> Col -> [String] -> [String]
breakDownAt _ _ [] =
  []

breakDownAt 0 col (x:xs) =
  take col x : drop col x : xs

breakDownAt row col (x:xs) =
  x : breakDownAt (row - 1) col xs


joinLinesUp :: Row -> [String] -> [String]
joinLinesUp _ [] =
  []

joinLinesUp _ [x1] =
  [x1]

joinLinesUp 1 (x1:x2:xs) =
  x1 <> x2 : xs

joinLinesUp row (x:xs) =
  x : joinLinesUp (row - 1) xs
