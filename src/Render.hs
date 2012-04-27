module Render where

import Control.Monad
import Graphics.Rendering.Cairo hiding (status, Status)
import Job
import Layout
import Monitor
import Rect

renderScreen :: [Monitor] -> Double -> Double -> Render ()
renderScreen monitors w h = do
    renderBackground
    renderJobs monitors (shrink 0.25 $ Rect 0 0 w h)

renderBackground :: Render ()
renderBackground = do
    setSourceRGB 1 1 1
    paint

renderJobs :: [Monitor] -> Rect -> Render ()
renderJobs monitors rect = do
    let rects = map (shrink 0.5) (findRects rect monitors)
    forM_ (zip monitors rects) renderJob

renderJob :: (Monitor, Rect) -> Render ()
renderJob (JobMonitor { mJobName = name, mJobStatus = status, mSecondsInState = t }, Rect x y w h) = do
    newPath
    let (r, g, b, a) = color t status
    setSourceRGBA r g b a
    rectangle x y w h
    fill

    setSourceRGBA 0 0 0 1
    moveTo (x + 10) (y+20)
    showText name

    let errors = errorText status
    let ys = map (\x -> fromIntegral x*10 + y + 20) [1..length errors]
    forM_ (zip errors ys) $ \(e, y) -> do
        moveTo (x + 20) y
        showText e
    return ()

circularMovement :: Double -> Double -> Double -> Double -> Double
circularMovement start end animationTime totalTime = res
    where
        rest    = totalTime - animationTime * fromIntegral (floor $ totalTime / animationTime)
        percent = rest / animationTime
        d1      = end - start
        d2      = start - end
        p2      = percent * 2
        res     = if p2 <= 1
                      then start + d1 * percent
                      else end   + d2 * percent

color :: Double -> Status -> (Double, Double, Double, Double)
color t Idle     = (0, 1, 0, 1)
color t Working  = (0, 1, 1, circularMovement 1 0.5 2 t)
color t (Fail _) = (1, 0, 0, circularMovement 1 0.5 0.5 t)

errorText :: Status -> [String]
errorText (Fail s) = lines s
errorText _        = []
