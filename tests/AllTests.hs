import Asserts
import Data.IORef
import Fixtures
import Notifier
import Rect
import System.Directory
import System.FilePath
import Test.Hspec.HUnit()
import Test.Hspec.Monadic
import Test.Hspec.QuickCheck
import Test.QuickCheck

main = hspecX $ do

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

    describe "rectangle operations:" $

        let area  (Rect x y w h) = w * h
            areas rects          = sum $ map area rects
            aboutSame d1 d2      = abs (d1 - d2) < 0.00000001
        in do

        prop "divideVertical preserves total area" $ forAll (choose (0, 1)) $ \percent ->
                                                     forAll arbitrary       $ \rect ->
            let (r1, r2) = divideVertical rect percent in
            area rect `aboutSame` areas [r1, r2]

        prop "splitVertical preserves total area" $ forAll arbitrary        $ \rect ->
                                                    forAll (choose (1, 10)) $ \numTimes ->
            area rect `aboutSame` areas (splitVertical rect numTimes)

        prop "splitHorizontal preserves total area" $ forAll arbitrary        $ \rect ->
                                                      forAll (choose (1, 10)) $ \numTimes ->
            area rect `aboutSame` areas (splitHorizontal rect numTimes)
