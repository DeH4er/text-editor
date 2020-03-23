module Core.Cursor
  ( Cursor
  , Row
  , Col
  , getCol
  , getRow
  , mkCursor
  ) where


type Row = Int


type Col = Int


newtype Cursor = Cursor (Row, Col)


getRow :: Cursor -> Row
getRow (Cursor (row, _)) =
  row


getCol :: Cursor -> Col
getCol (Cursor (_, col)) =
  col


mkCursor :: Row -> Col -> Cursor
mkCursor row col =
  Cursor (row, col)

