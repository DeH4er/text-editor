import           System.Environment
import           Ui (startUi)
import           Core (handleIO, openEvent, initApp)


main = do
  args <- getArgs
  case args of
    [filePath] ->
      openFile filePath
    _ ->
      openEmpty


openFile :: FilePath -> IO ()
openFile filepath = do
  newApp <- handleIO openFileEvent initApp
  startUi newApp
    where
      openFileEvent = openEvent filepath


openEmpty :: IO ()
openEmpty =
  startUi initApp
