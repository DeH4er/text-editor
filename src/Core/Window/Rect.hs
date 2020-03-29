module Core.Window.Rect
  ( Rect(..)
  , Width
  , Height
  , new
  , empty
  , resize
  , resizeWidth
  , resizeHeight
  , translate
  , translateRow
  , translateCol
  )
where


import qualified Core.Cursor as Cursor


type Width =
  Int


type Height =
  Int


data Rect
  = Rect
    { rectCol :: Cursor.Col
    , rectRow :: Cursor.Row
    , rectWidth :: Width
    , rectHeight :: Height
    }
    deriving (Show, Eq)


empty :: Rect
empty =
  new 0 0 0 0


new :: Cursor.Row -> Cursor.Col -> Width -> Height -> Rect
new row col width height =
  Rect
  { rectCol = col
  , rectRow = row
  , rectWidth = width
  , rectHeight = height
  }


resize :: Width -> Height -> Rect -> Rect
resize width height rect =
  rect {rectWidth = width, rectHeight = height}


resizeWidth :: Width -> Rect -> Rect
resizeWidth width rect =
  rect {rectWidth = width}


resizeHeight :: Height -> Rect -> Rect
resizeHeight height rect =
  rect {rectHeight = height}


translate :: Cursor.Row -> Cursor.Col -> Rect -> Rect
translate row col rect =
  rect {rectCol = col, rectRow = row}


translateRow :: Cursor.Row -> Rect -> Rect
translateRow row rect =
  rect {rectRow = row}


translateCol :: Cursor.Col -> Rect -> Rect
translateCol col rect =
  rect {rectCol = col}
