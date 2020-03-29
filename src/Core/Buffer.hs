module Core.Buffer
  ( Buffer
  , getContent
  , getFilepath
  , getCursor
  , empty
  , loadContent
  , moveCursor
  , insertChar
  , breakLine
  , deleteChar
  )
where

import qualified Core.Cursor as Cursor
import Core.Utils
import Core.MoveAction


data Buffer =
  Buffer
  { bufFilepath :: Maybe FilePath
  , bufCursor :: Cursor.Cursor
  , bufContent :: [String]
  }
  deriving (Show, Eq)


getContent :: Buffer -> [String]
getContent =
  bufContent


getFilepath :: Buffer -> Maybe FilePath
getFilepath =
  bufFilepath


getCursor :: Buffer -> Cursor.Cursor
getCursor =
  bufCursor


empty :: Buffer
empty =
  Buffer
  { bufFilepath = Nothing
  , bufCursor = Cursor.empty
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
  buffer { bufCursor = Cursor.new croppedRow croppedCol }
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
        Cursor.getRowCol $ bufCursor buffer


insertChar :: Buffer -> Char -> Buffer
insertChar buffer char = newBuffer
  where
    newBuffer :: Buffer
    newBuffer =
      buffer { bufCursor = newCursor , bufContent = newContent }

    newCursor :: Cursor.Cursor
    newCursor =
      Cursor.new row (col + 1)

    newContent :: [String]
    newContent =
      modifyAt row (insertAt col char) content

    content :: [String]
    content =
      bufContent buffer

    (row, col) =
      Cursor.getRowCol $ bufCursor buffer


deleteChar :: Buffer -> Buffer
deleteChar buffer = newBuffer
  where
    newBuffer :: Buffer
    newBuffer =
      buffer { bufCursor = newCursor, bufContent = newContent}

    newCursor :: Cursor.Cursor
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
           in Cursor.new newRow newCol
        else
          Cursor.new row (cropMin 0 (col - 1))

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
      Cursor.getRowCol $ bufCursor buffer


breakLine :: Buffer -> Buffer
breakLine buffer = newBuffer
  where
    newBuffer :: Buffer
    newBuffer =
      buffer { bufCursor = newCursor, bufContent = newContent}

    newCursor :: Cursor.Cursor
    newCursor =
      Cursor.new (row + 1) 0

    newContent :: [String]
    newContent =
      breakDownAt row col content

    content :: [String]
    content =
      bufContent buffer

    (row, col) =
      Cursor.getRowCol $ bufCursor buffer


breakDownAt :: Cursor.Row -> Cursor.Col -> [String] -> [String]
breakDownAt _ _ [] =
  []

breakDownAt 0 col (x:xs) =
  take col x : drop col x : xs

breakDownAt row col (x:xs) =
  x : breakDownAt (row - 1) col xs


joinLinesUp :: Cursor.Row -> [String] -> [String]
joinLinesUp _ [] =
  []

joinLinesUp _ [x1] =
  [x1]

joinLinesUp 1 (x1:x2:xs) =
  x1 <> x2 : xs

joinLinesUp row (x:xs) =
  x : joinLinesUp (row - 1) xs
