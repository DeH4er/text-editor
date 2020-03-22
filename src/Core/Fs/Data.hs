module Core.Fs.Data
  ( FileContent(..)
  , FsResult(..)
  , FsErr(..)
  , FsService(..)
  )
where


type FileContent = String


type FsResult a = Either FsErr a


data FsErr = PathDoesNotExist
           | Unknown


class Monad m => FsService m where
  loadFile :: FilePath -> m (FsResult FileContent)
  saveFile :: FilePath -> FileContent -> m (FsResult ())
