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
  newApp <- handle openFile initApp
  startUi newApp
    where
      openFile = openEvent filepath


openEmpty :: IO ()
openEmpty =
  startUi initApp
