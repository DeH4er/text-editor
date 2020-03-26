module Core.Cursor
  ( Cursor
  , Row
  , Col
  , getCol
  , getRow
  , getRowCol
  , mkCursor
  ) where


type Row =
  Int


type Col =
  Int


newtype Cursor =
  Cursor (Row, Col)
  deriving (Eq, Show)


getRowCol :: Cursor -> (Row, Col)
getRowCol (Cursor p) = p


getRow :: Cursor -> Row
getRow (Cursor (row, _)) =
  row


getCol :: Cursor -> Col
getCol (Cursor (_, col)) =
  col


mkCursor :: Row -> Col -> Cursor
mkCursor row col =
  Cursor (row, col)

