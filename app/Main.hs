import           System.Environment
import           Ui (startUi)
import           Core (handle, openEvent, initApp)


main = do
  args <- getArgs
  case args of
    [filePath] ->
      openFile filePath
    _ ->
      openEmpty


openFile :: FilePath -> IO ()
openFile filepath = do
  newApp <- handle event initApp
  startUi newApp
    where
      event = openEvent filepath


openEmpty :: IO ()
openEmpty =
  startUi initApp
