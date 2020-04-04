module Core.Movement
  ( moveForwardWord
  , moveForwardEndWord
  , moveEndLine
  , moveStartLine
  )
where


import qualified Core.Cursor as Cursor
import Core.Cursor (Cursor, Row, Col)

import qualified Core.Utils as Utils
import Data.Maybe (fromMaybe)
import Control.Applicative ((<|>))


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


moveEndLine :: Content -> Modify Cursor
moveEndLine content =
  Cursor.modify $ \(row, col) -> (row, getEndLine content row)
    where
      getEndLine :: Content -> Row -> Col
      getEndLine content row =
        length $ getRow content row


moveStartLine :: Modify Cursor
moveStartLine =
  Cursor.modify $ \(row, col) -> (row, 0)


moveBackwardWord :: Content -> Modify Cursor
moveBackwardWord =
  fromMaybe moveStartRow $ movePrevWord <|> movePrevLine
    where
      moveStartRow =
        undefined

      movePrevWord =
        undefined

      movePrevLine =
        undefined


moveForwardEndWord :: Content -> Modify Cursor
moveForwardEndWord content cursor =
  fromMaybe moveEndRow $ moveNextWord <|> moveNextLine
    where
      moveNextWord :: Maybe Cursor
      moveNextWord =
        findNextAtRow findNextWord content cursor

      moveNextLine :: Maybe Cursor
      moveNextLine =
        if isLastRow content cursor
          then
            Nothing
          else
            Just
            . moveForwardEndWord content
            . moveStartLine
            . moveBottom content
            $ cursor

      moveEndRow :: Cursor
      moveEndRow =
        moveEndLine content cursor

      findNextWord :: Col -> String -> Maybe Col
      findNextWord col str = do
        i1 <- findNonSpace (col + 1) str
        let i2 = findCurrentClassEnd (getCharClass $ str !! i1) i1 str
        return $ i2 - 1


moveForwardWord :: Content -> Modify Cursor
moveForwardWord content cursor =
  fromMaybe moveEndRow $ moveNextWord <|> moveNextLine
    where
      moveNextWord :: Maybe Cursor
      moveNextWord =
        findNextAtRow findNextWord content cursor

      moveNextLine :: Maybe Cursor
      moveNextLine =
        if isLastRow content cursor
          then
            Nothing
          else
            findNextAtRow findNonSpace content
            . moveStartLine
            . moveBottom content
            $ cursor

      moveEndRow :: Cursor
      moveEndRow =
        moveEndLine content cursor

      findNextWord :: Col -> String -> Maybe Col
      findNextWord _ [] = Nothing
      findNextWord col str =
        case drop col str of
          [] ->
            Nothing
          x : _ -> do
            i <- findDifferentClass (getCharClass x) (col + 1) str
            findNonSpace i str


findNextAtRow :: (Col -> String -> Maybe Col) -> Content -> Cursor -> Maybe Cursor
findNextAtRow f content cursor = do
  rowStr <- getMaybeRow content row
  newCol <- f col rowStr
  return $ moveToCol newCol cursor
  where
    moveToCol :: Col -> Modify Cursor
    moveToCol newCol =
      Cursor.modify $ \(row, col) -> (row, newCol)

    (row, col) =
      Cursor.getRowCol cursor


findDifferentClass :: CharClass -> Col -> String -> Maybe Col
findDifferentClass cls col str = do
  i <- Utils.findIndex (\x -> getCharClass x /= cls) $ drop col str
  return $ i + col


findCurrentClassEnd :: CharClass -> Col -> String -> Col
findCurrentClassEnd cls col str =
  fromMaybe (length str) $ findDifferentClass cls col str


findNonSpace :: Col -> String -> Maybe Col
findNonSpace col str = do
  i <- Utils.findIndex (\x -> getCharClass x /= WhiteSpace) $ drop col str
  return $ i + col


isLastRow :: Content -> Cursor -> Bool
isLastRow content cursor =
  length content - 1 == Cursor.getRow cursor


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
