module Core.Deletion
  ( delete
  , forwardWord
  , deleteBetweenCursors
  )
where


import qualified Core.Movement as Movement
import Core.Movement (Movement(..))

import qualified Core.Cursor as Cursor
import Core.Cursor (Cursor, Row, Col)

import qualified Core.Utils as Utils
import Core.Utils (Content, Modify)


delete :: Movement -> Content -> Cursor -> (Content, Cursor)
delete MForwardWord =
  forwardWord


forwardWord :: Content -> Cursor -> (Content, Cursor)
forwardWord content start =
  (newContent, start)
    where
      newContent =
        deleteBetweenCursors start end content

      end =
        Movement.forwardEndWord content start


deleteBetweenCursors :: Cursor -> Cursor -> Modify Content
deleteBetweenCursors start end content =
  if Movement.isLastCol content end
    then
      if Movement.isLastRow content end
        then
          Utils.removeRange (sRow + 1) (eRow + 1)
          . Utils.modifyAt sRow (take sCol)
          $ content
      else
        Utils.removeRange (sRow + 1) (eRow + 1)
        . Utils.modifyAt sRow (\row -> take sCol row <> content !! (eRow + 1))
        $ content
    else
      Utils.removeRange (sRow + 1) eRow
      . Utils.modifyAt sRow (\row -> take sCol row <> drop (eCol + 1) (content !! eRow))
      $ content
  where
    (sRow, sCol) = Cursor.getRowCol start
    (eRow, eCol) = Cursor.getRowCol end
