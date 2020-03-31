import           System.Environment
import           Ui (startUi)
import           Core (interpretActionIO, openEvent, initApp)
import           Core.Key


main = do
  args <- getArgs
  case args of
    [filePath] ->
      openFile filePath
    _ ->
      openEmpty


openFile :: FilePath -> IO ()
openFile filepath = do
  newApp <- interpretActionIO openFileAction initApp
  startUi newApp
    where
      openFileAction = OpenFile filepath


openEmpty :: IO ()
openEmpty =
  startUi initApp
