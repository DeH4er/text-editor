module Core.Cursor
  ( Cursor
  , Row
  , Col
  , getCol
  , getRow
  , getRowCol
  , new
  , empty
  , modify
  )
where


type Row =
  Int


type Col =
  Int


newtype Cursor =
  Cursor (Row, Col)
  deriving (Eq, Show)


modify :: ((Row, Col) -> (Row, Col)) -> Cursor -> Cursor
modify f = Cursor . f . getRowCol


getRowCol :: Cursor -> (Row, Col)
getRowCol (Cursor p) =
  p


getRow :: Cursor -> Row
getRow (Cursor (row, _)) =
  row


getCol :: Cursor -> Col
getCol (Cursor (_, col)) =
  col


new :: Row -> Col -> Cursor
new row col =
  Cursor (row, col)


empty :: Cursor
empty =
  new 0 0
