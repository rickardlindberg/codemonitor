module Main (main) where

import Graphics.Rendering.Cairo
import Graphics.UI.Gtk

main :: IO ()
main = do
    initGUI
    showMainWindow
    mainGUI

showMainWindow :: IO ()
showMainWindow = do
    builder    <- builderFromFile "interface.glade"
    mainWindow <- builderGetObject builder castToWindow "main_window"
    canvas     <- builderGetObject builder castToDrawingArea "canvas"
    mainWindow `onDestroy` mainQuit
    canvas     `onExpose`  redraw canvas
    widgetShowAll mainWindow

builderFromFile :: FilePath -> IO Builder
builderFromFile path = do
    builder <- builderNew
    builderAddFromFile builder path
    return builder

redraw canvas event = do
    (w, h) <- widgetGetSize canvas
    drawin <- widgetGetDrawWindow canvas
    renderWithDrawable drawin (myDraw (fromIntegral w) (fromIntegral h))
    return True

myDraw :: Double -> Double -> Render ()
myDraw w h = do
    setSourceRGB 1 1 1
    paint

    setSourceRGB 0 0 0
    moveTo 0 0
    lineTo w h
    moveTo w 0
    lineTo 0 h
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
