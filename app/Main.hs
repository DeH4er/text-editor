import           System.Environment
import           Ui (startUi)
import           Core (interpretActionIO, openEvent, empty)
import           Core.Mode


main = do
  args <- getArgs
  case args of
    [filePath] ->
      openFile filePath
    _ ->
      openEmpty


openFile :: FilePath -> IO ()
openFile filepath = do
  newApp <- interpretActionIO openFileAction empty
  startUi newApp
    where
      openFileAction = OpenFile filepath


openEmpty :: IO ()
openEmpty =
  startUi empty
