module Core.CursorUtils
  ( moveForwardWord
  )
where


import qualified Core.Cursor as Cursor
import Core.Cursor (Cursor, Row, Col)

import qualified Core.Utils as Utils
import Data.Maybe (fromMaybe)

import Text.Regex.TDFA


type Content = [String]


type Modify a = a -> a


moveLeft :: Content -> Modify Cursor
moveLeft =
  modifyCrop $ \(row, col) -> (row, col - 1)


moveRight :: Content -> Modify Cursor
moveRight =
  modifyCrop $ \(row, col) -> (row, col - 1)


moveTop :: Content -> Modify Cursor
moveTop =
  modifyCrop $ \(row, col) -> (row - 1, col)


moveBottom :: Content -> Modify Cursor
moveBottom =
  modifyCrop $ \(row, col) -> (row + 1, col)


moveEndRow :: Content -> Modify Cursor
moveEndRow content =
  Cursor.modify $ \(row, col) -> (row, getEndLine content row)
    where
      getEndLine :: Content -> Row -> Col
      getEndLine content row =
        length $ getRow content row


moveStartRow :: Modify Cursor
moveStartRow =
  Cursor.modify $ \(row, col) -> (row, 0)


moveBackwardWord :: Content -> Modify Cursor
moveBackwardWord =
  undefined


moveForwardEndWord :: Content -> Modify Cursor
moveForwardEndWord =
  undefined


moveForwardWord :: Content -> Modify Cursor
moveForwardWord content cursor =
  fromMaybe defaultCursor $ findNextCursorAtRow doFindNextWord content cursor
    where
      doFindNextWord :: Col -> String -> Maybe Col
      doFindNextWord _ [] = Nothing
      doFindNextWord col str =
        case  drop col str of
          [] ->
            Nothing
          x : xs ->
            find (getCharClass x) (col + 1) xs

      findNonSpace :: Col -> String -> Maybe Col
      findNonSpace col [] =
        Nothing

      findNonSpace col (x:xs) =
        if getCharClass x /= WhiteSpace
          then
            Just col
          else
            findNonSpace (col + 1) xs

      find :: CharClass -> Col -> String -> Maybe Col
      find _ _ [] =
        Nothing

      find cls col (x:xs) =
        if cls /= getCharClass x
          then
            Just  col
          else
            find cls (col + 1) xs

      defaultCursor :: Cursor
      defaultCursor =
        if isLastRow content cursor
          then
            moveEndRow content cursor
          else
            let newCursor = moveStartRow . moveBottom content $ cursor
            in fromMaybe newCursor $ findNextCursorAtRow findNonSpace content newCursor


isLastRow :: Content -> Cursor -> Bool
isLastRow content cursor =
  length content - 1 == Cursor.getRow cursor


findNextCursorAtRow :: (Col -> String -> Maybe Col) -> Content -> Cursor -> Maybe Cursor
findNextCursorAtRow f content cursor = do
  rowStr <- getMaybeRow content row
  newCol <- f col rowStr
  return $ moveToCol newCol cursor
  where
    moveToCol :: Col -> Modify Cursor
    moveToCol newCol =
      Cursor.modify $ \(row, col) -> (row, newCol)

    (row, col) =
      Cursor.getRowCol cursor


findNextPosition :: (Char -> Bool) -> Int -> String -> Maybe Int

findNextPosition f pos [] =
  Nothing

findNextPosition f pos (x:xs) =
  if not $ f x
    then
      findNextPosition f pos xs
    else
      Just pos


modifyCrop :: Modify (Row, Col) -> Content -> Modify Cursor
modifyCrop f content =
  cropCursor content . Cursor.modify f


getRow :: Content -> Row -> String
getRow content row
  | row < 0 =
    head content

  | row > length content =
    last content

  | otherwise =
    content !! row


getMaybeRow :: Content -> Row -> Maybe String
getMaybeRow content row
  | row > length content =
    Nothing

  | otherwise =
    Just $ content !! row


cropCursor :: Content -> Modify Cursor
cropCursor content =
  Cursor.modify doModify
    where
      doModify (row, col) =
        (newRow, newCol)
          where
            newRow =
              Utils.crop 0 (length content - 1) row

            newCol =
              Utils.crop 0 (length $ content !! newRow) col


data CharClass =
  Punctuation
  | WhiteSpace
  | OtherClass
  deriving (Show, Eq)


getCharClass :: Char -> CharClass
getCharClass ',' = Punctuation
getCharClass '.' = Punctuation
getCharClass '!' = Punctuation
getCharClass '?' = Punctuation
getCharClass '"' = Punctuation
getCharClass '\'' = Punctuation
getCharClass '=' = Punctuation
getCharClass '-' = Punctuation
getCharClass ':' = Punctuation
getCharClass '{' = Punctuation
getCharClass '}' = Punctuation
getCharClass '(' = Punctuation
getCharClass ')' = Punctuation
getCharClass '|' = Punctuation
getCharClass '$' = Punctuation
getCharClass '%' = Punctuation
getCharClass '^' = Punctuation
getCharClass '&' = Punctuation
getCharClass '*' = Punctuation
getCharClass '\\' = Punctuation
getCharClass '/' = Punctuation
getCharClass '~' = Punctuation

getCharClass ' ' = WhiteSpace
getCharClass '\t' = WhiteSpace

getCharClass _ = OtherClass
