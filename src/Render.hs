module Render where

import Control.Monad
import Graphics.Rendering.Cairo hiding (status)
import Job
import Layout
import Rect

renderScreen :: [Job] -> Double -> Double -> Render ()
renderScreen jobs w h = do
    renderBackground
    renderJobs jobs (shrink 0.25 $ Rect 0 0 w h)

renderBackground :: Render ()
renderBackground = do
    setSourceRGB 1 1 1
    paint

renderJobs :: [Job] -> Rect -> Render ()
renderJobs jobs rect = do
    let rects = map (shrink 0.5) (findRects rect jobs)
    forM_ (zip jobs rects) renderJob

renderJob :: (Job, Rect) -> Render ()
renderJob (job, Rect x y w h) = do
    newPath
    let (r, g, b, a) = color job
    setSourceRGBA r g b a
    rectangle x y w h
    fill

    setSourceRGBA 0 0 0 1
    moveTo (x + 10) (y+20)
    showText (fullName job)

    let errors = errorText job
    let ys = map (\x -> fromIntegral x*10 + y + 20) [1..length errors]
    forM_ (zip errors ys) $ \(e, y) -> do
        moveTo (x + 20) y
        showText e
    return ()

color :: Job -> (Double, Double, Double, Double)
color (Job { status = Idle }) = (0, 1, 0, 1)
color (Job { status = Working }) = (0, 1, 1, 1)
color (Job { status = Fail _ }) = (1, 0, 0, 1)

errorText :: Job -> [String]
errorText (Job { status = Fail s }) = lines s
errorText _ = []
