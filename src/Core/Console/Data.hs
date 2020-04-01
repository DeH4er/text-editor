module Core.Console.Data
  ( Console
  , Position
  , empty
  , getPosition
  , getContent
  , modifyPosition
  , modifyContent
  , modifyContentByPosition
  , modifyPositionByContent
  )
where


type Position =
  Int


data Console
  = Console
  { conPosition :: Position
  , conContent :: String
  }
  deriving (Show, Eq)


empty :: Console
empty =
  Console
  { conPosition = 0
  , conContent = ""
  }


getPosition :: Console -> Position
getPosition =
  conPosition


getContent :: Console -> String
getContent =
  conContent


modifyPosition :: (Position -> Position) -> Console -> Console
modifyPosition f con =
  con { conPosition = f . getPosition $ con}


modifyContent :: (String -> String) -> Console -> Console
modifyContent f con =
  con { conContent = f . getContent $ con}


modifyContentByPosition :: (Position -> String -> String) -> Console -> Console
modifyContentByPosition f con =
  modifyContent (f $ getPosition con) con


modifyPositionByContent :: (String -> Position -> Position) -> Console -> Console
modifyPositionByContent f con =
  modifyPosition (f $ getContent con) con
