module Main (main) where

import Data.IORef
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk
import System.INotify

main :: IO ()
main = do
    initGUI
    showMainWindow
    mainGUI

showMainWindow :: IO ()
showMainWindow = do
    iRef       <- newIORef 0
    builder    <- builderFromFile "interface.glade"
    mainWindow <- builderGetObject builder castToWindow "main_window"
    canvas     <- builderGetObject builder castToDrawingArea "canvas"
    mainWindow `onDestroy` mainQuit
    canvas     `onExpose`  redraw canvas iRef
    widgetShowAll mainWindow
    let x = do
        modifyIORef iRef (+1)
        i <- readIORef iRef
        widgetQueueDraw canvas
        return True
    timeoutAdd x 10
    testINotify
    return ()

testINotify = do
    i <- initINotify
    addWatch i [Modify, MoveIn, MoveOut] "src" $ \e -> do
        putStrLn $ "something changed: " ++ (show e)
        return ()
    return ()

builderFromFile :: FilePath -> IO Builder
builderFromFile path = do
    builder <- builderNew
    builderAddFromFile builder path
    return builder

redraw canvas iRef event = do
    i <- readIORef iRef
    (w, h) <- widgetGetSize canvas
    drawin <- widgetGetDrawWindow canvas
    renderWithDrawable drawin (myDraw i (fromIntegral w) (fromIntegral h))
    return True

myDraw :: Integer -> Double -> Double -> Render ()
myDraw i w h = do
    setSourceRGB 1 1 1
    paint

    let deltaW = fromIntegral (i `mod` floor w)
    let deltaH = fromIntegral (i `mod` floor h)

    let x1 = 0 + deltaW
    let x2 = w - deltaW
    let y1 = 0 + deltaH
    let y2 = h - deltaH

    setSourceRGB 0 0 0
    moveTo x1 0
    lineTo x2 h
    moveTo w y1
    lineTo 0 y2
    setLineWidth (0.1 * (h + w))
    stroke

    rectangle 0 0 (0.5 * w) (0.5 * h)
    setSourceRGBA 1 0 0 0.8
    fill

    rectangle 0 (0.5 * h) (0.5 * w) (0.5 * h)
    setSourceRGBA 0 1 0 0.6
    fill

    rectangle (0.5 * w) 0 (0.5 * w) (0.5 * h)
    setSourceRGBA 0 0 1 0.4
    fill
