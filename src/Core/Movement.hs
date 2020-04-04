module Core.Movement
  ( backwardRow
  , forwardRow
  , move
  , backwardWord
  , forwardWord
  , forwardEndWord
  , endLine
  , startLine
  , Movement (..)
  )
where


import qualified Core.Cursor as Cursor
import Core.Cursor (Cursor, Row, Col)

import qualified Core.Utils as Utils
import Data.Maybe (fromMaybe)
import Control.Applicative ((<|>))


data Movement =
  MTop
  | MBottom
  | MLeft
  | MRight
  | MForwardWord
  | MForwardEndWord
  | MBackwardWord
  | MEndLine
  | MStartLine
  | MEndContent
  | MStartContent
  | MForwardHalfScreen
  | MBackwardHalfScreen
  deriving (Show, Eq)


type Content = [String]


type Modify a = a -> a


move :: Movement -> Content -> Modify Cursor
move MTop =
  top

move MBottom =
  bottom

move MLeft =
  left

move MRight =
  right

move MForwardWord =
  forwardWord

move MForwardEndWord =
  forwardEndWord

move MBackwardWord =
  backwardWord

move MEndLine =
  endLine

move MStartLine =
  const startLine

move MEndContent =
  endContent

move MStartContent =
  startContent


left :: Content -> Modify Cursor
left =
  modifyCrop $ \(row, col) -> (row, col - 1)


right :: Content -> Modify Cursor
right =
  modifyCrop $ \(row, col) -> (row, col + 1)


top :: Content -> Modify Cursor
top =
  modifyCrop $ \(row, col) -> (row - 1, col)


bottom :: Content -> Modify Cursor
bottom =
  modifyCrop $ \(row, col) -> (row + 1, col)


endLine :: Content -> Modify Cursor
endLine content =
  Cursor.modify $ \(row, col) -> (row, getEndLine content row)
    where
      getEndLine :: Content -> Row -> Col
      getEndLine content row =
        length $ getRow content row


startLine :: Modify Cursor
startLine =
  Cursor.modify $ \(row, col) -> (row, 0)


backwardWord :: Content -> Modify Cursor
backwardWord content cursor =
  fromMaybe (Cursor.new 0 0) $ movePrevWord <|> movePrevLine
    where
      movePrevWord :: Maybe Cursor
      movePrevWord =
        findAtRow findPrevWord content cursor

      movePrevLine :: Maybe Cursor
      movePrevLine =
        if isFirstRow cursor
          then
            Nothing
          else
            let newCursor = endLine content . top content $ cursor
            in Just
            . fromMaybe newCursor
            . findAtRow findPrevWord content
            $ newCursor

      findPrevWord :: Col -> String -> Maybe Col
      findPrevWord origCol origStr = do
        let
          str = reverse origStr
          col = length str - origCol

        i <- findWordEnd col str
        return $ length str - i - 1


forwardEndWord :: Content -> Modify Cursor
forwardEndWord content cursor =
  fromMaybe moveEndRow $ moveNextWord <|> moveNextLine
    where
      moveNextWord :: Maybe Cursor
      moveNextWord =
        findAtRow findWordEnd content cursor

      moveNextLine :: Maybe Cursor
      moveNextLine =
        if isLastRow content cursor
          then
            Nothing
          else
            Just
            . forwardEndWord content
            . startLine
            . bottom content
            $ cursor

      moveEndRow :: Cursor
      moveEndRow =
        endLine content cursor


forwardWord :: Content -> Modify Cursor
forwardWord content cursor =
  fromMaybe moveEndRow $ moveNextWord <|> moveNextLine
    where
      moveNextWord :: Maybe Cursor
      moveNextWord =
        findAtRow findNextWord content cursor

      moveNextLine :: Maybe Cursor
      moveNextLine =
        if isLastRow content cursor
          then
            Nothing
          else
            let newCursor = startLine . bottom content $ cursor
            in Just
            . fromMaybe newCursor
            . findAtRow findNonSpace content
            $ newCursor

      moveEndRow :: Cursor
      moveEndRow =
        endLine content cursor

      findNextWord :: Col -> String -> Maybe Col
      findNextWord _ [] = Nothing
      findNextWord col str =
        case drop col str of
          [] ->
            Nothing
          x : _ -> do
            i <- findDifferentClass (getCharClass x) (col + 1) str
            findNonSpace i str


endContent :: Content -> Modify Cursor
endContent content cursor =
  fromMaybe lastRow $ findAtRow findNonSpace content lastRow
    where
      lastRow :: Cursor
      lastRow =
        Cursor.new (length content - 1) 0


startContent :: Content -> Modify Cursor
startContent content =
  const $ fromMaybe firstRow $ findAtRow findNonSpace content firstRow
    where
      firstRow :: Cursor
      firstRow =
        Cursor.new 0 0


forwardRow :: Int -> Content -> Modify Cursor
forwardRow i content cursor =
  let newCursor = modifyCrop (\(row, col) -> (row + i, 0)) content cursor
  in fromMaybe newCursor $ findAtRow findNonSpace content newCursor


backwardRow :: Int -> Content -> Modify Cursor
backwardRow i =
  forwardRow $ -i


findWordEnd :: Col -> String -> Maybe Col
findWordEnd col str = do
  i1 <- findNonSpace (col + 1) str
  let i2 = findCurrentClassEnd (getCharClass $ str !! i1) i1 str
  return $ i2 - 1


findAtRow :: (Col -> String -> Maybe Col) -> Content -> Cursor -> Maybe Cursor
findAtRow f content cursor = do
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


isFirstRow :: Cursor -> Bool
isFirstRow =
  (== 0) . Cursor.getRow


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
getCharClass '\'' = Punctuation
getCharClass '\\' = Punctuation
getCharClass ',' = Punctuation
getCharClass '.' = Punctuation
getCharClass '!' = Punctuation
getCharClass '?' = Punctuation
getCharClass '"' = Punctuation
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
getCharClass '/' = Punctuation
getCharClass '~' = Punctuation

getCharClass ' ' = WhiteSpace
getCharClass '\t' = WhiteSpace

getCharClass _ = OtherClass
