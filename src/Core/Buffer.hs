module Core.Buffer
  ( Buffer
  , getContent
  , getFilepath
  , empty
  , loadContent
  , modifyContent
  , fromContent
  )
where

import qualified Core.Cursor as Cursor
import Core.Utils


data Buffer =
  Buffer
  { bufFilepath :: Maybe FilePath
  , bufContent :: [String]
  }
  deriving (Show, Eq)


getContent :: Buffer -> [String]
getContent =
  bufContent


getFilepath :: Buffer -> Maybe FilePath
getFilepath =
  bufFilepath


empty :: Buffer
empty =
  Buffer
  { bufFilepath = Nothing
  , bufContent = [""]
  }


fromContent :: FilePath -> [String] -> Buffer
fromContent =
  loadContent empty

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
