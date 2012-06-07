import Fixtures
import Job.Types
import Monitor
import Render.Layout
import Render.Rect
import System.Directory
import System.FilePath
import Test.Hspec.HUnit()
import Test.Hspec.Monadic
import Test.Hspec.QuickCheck
import Test.HUnit
import Test.QuickCheck

main = hspecX $ do

    describe "notification service:" $ do

        it "notifies when file modified in directory" $ withTmpDir $ \dir -> do
            assertNotified <- setupNotificationsTest dir
            modifyFile $ dir </> "foo"
            assertNotified "foo"

        it "notifies when file modified in subdirectory" $ withTmpDir $ \dir -> do
            createDirectory $ dir </> "subdir"
            assertNotified <- setupNotificationsTest dir
            modifyFile $ dir </> "subdir" </> "foo"
            assertNotified ("subdir" </> "foo")

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

    describe "ordering monitors:" $ do

        describe "does not change when:" $

            it "no monitors require large space failing" $ do
                let monitors = [ StatusCodeMonitor "" 0 "" Idle ""
                               , StatusCodeMonitor "" 0 "" Working ""
                               ]
                orderMonitors monitors @?= monitors

        describe "does change when:" $

            it "a monitor require large space" $ do
                let monitors = [ StatusCodeMonitor "" 0 "" Idle ""
                               , StatusCodeMonitor "" 0 "" Working ""
                               , StdoutMonitor "" 0 "" ""
                               ]
                let expected = [ StdoutMonitor "" 0 "" ""
                               , StatusCodeMonitor "" 0 "" Idle ""
                               , StatusCodeMonitor "" 0 "" Working ""
                               ]
                orderMonitors monitors @?= expected
