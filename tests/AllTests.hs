import Asserts
import Data.IORef
import Fixtures
import Notifier
import System.Directory
import System.FilePath
import Test.Hspec.HUnit()
import Test.Hspec.Monadic

main = hspecX $

    describe "notification service" $ do

        it "file modified in directory" $ withTmpDir $ \dir -> do
            filesRef <- newIORef []
            setupNotifications dir (\f -> modifyIORef filesRef (f:))

            touch $ dir </> "foo"
            assertGotFile filesRef "foo"

        it "file modified in subdirectory" $ withTmpDir $ \dir -> do
            createDirectory $ dir </> "subdir"

            filesRef <- newIORef []
            setupNotifications dir (\f -> modifyIORef filesRef (f:))

            touch $ dir </> "subdir" </> "foo"
            assertGotFile filesRef "foo"
