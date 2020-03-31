module Core.Buffer
  ( Buffer
  , getContent
  , getFilepath
  , getCursor
  , empty
  , loadContent
  , modifyContent
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


modifyContent :: ([String] -> [String]) -> Buffer -> Buffer
modifyContent f buffer =
  buffer
  { bufContent = f . getContent $ buffer}
