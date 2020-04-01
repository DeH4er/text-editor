module Core.Console
  ( module Core.Console.Data
  , moveLeft
  , moveRight
  , deleteChar
  , insertChar
  , clearContent
  )
where


import qualified Core.Utils as Utils
import Core.Console.Data


insertChar :: Char -> Console -> Console
insertChar c =
  moveRight . withInsertedChar
    where
      withInsertedChar :: Console -> Console
      withInsertedChar =
        modifyContentByPosition doModify

      doModify :: Position -> String -> String
      doModify pos =
        Utils.insertAt pos c


deleteChar :: Console -> Console
deleteChar =
  moveLeft . withDeletedChar
    where
      withDeletedChar :: Console -> Console
      withDeletedChar =
        modifyContentByPosition doModify

      doModify :: Position -> String -> String
      doModify =
        Utils.removeAt


moveLeft :: Console -> Console
moveLeft =
  modifyPositionByContent doMove
    where
      doMove :: String -> Position -> Position
      doMove content position =
        cropPosition content (position - 1)


moveRight :: Console -> Console
moveRight =
  modifyPositionByContent doMove
    where
      doMove :: String -> Position -> Position
      doMove content position =
        cropPosition content (position + 1)


clearContent :: Console -> Console
clearContent =
   withModifiedPosition . withModifiedContent
  where
    withModifiedPosition =
      modifyPosition . const $ 0

    withModifiedContent =
      modifyContent . const $ ""


cropPosition :: String -> Position -> Position
cropPosition content =
  Utils.crop 0 (length content)
