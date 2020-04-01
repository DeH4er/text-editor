module Core.AppSpec
  ( spec
  )
where


import Test.Hspec
import Core.Fs.Data
import Core.App
import Core.Event


pureFsService :: Monad m => FsService m
pureFsService
  = FsService
  { loadFile = \filepath -> return . return $ ""
  , saveFile = \filepath -> return . return . return $ ()
  }


pureHandle
  :: Monad m
  => Key
  -> App
  -> m App
pureHandle = handle pureFsService


spec = do
  describe "Handle events" $ do
    it "should close app" $ do
      pending
      -- isAppClosed initApp `shouldBe` False
      -- let app = head $ pureHandle closeEvent initApp
      -- isAppClosed app `shouldBe` True
