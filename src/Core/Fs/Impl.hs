module Core.Fs.Impl
  ( ioFsService
  )
where


import System.IO
import Core.Fs.Data
import Control.Exception


ioFsService :: FsService IO
ioFsService = FsService
  { loadFile = loadFileImpl
  , saveFile = saveFileImpl
  }


loadFileImpl :: FilePath -> IO (FsResult FileContent)
loadFileImpl path =
  catchFs $ readFile path


saveFileImpl :: FilePath -> FileContent -> IO (FsResult ())
saveFileImpl path content =
  catchFs $ writeFile path content


catchFs :: IO a -> IO (FsResult a)
catchFs x = do
  res <- _try x
  return $ case res of
    Left _ ->
      Left Unknown

    Right value ->
      Right value

  where
    _try :: IO a -> IO (Either SomeException a)
    _try = try
