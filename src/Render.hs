module Render where

import Control.Monad
import Graphics.Rendering.Cairo hiding (status)
import Job

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

findRects :: Rect -> [Job] -> [Rect]
findRects rect jobs =
    if any isFailed jobs then
        let (top, bottom) = divideVertical rect 0.9
            tops = splitVertical top (count isFailed jobs)
            bottoms = splitHorizontal bottom (count (not . isFailed) jobs)
        in match tops bottoms jobs
    else
        splitVertical rect (length jobs)

match [] [] [] = []
match tops bottoms (j:js) =
    if isFailed j then
        head tops:match (tail tops) bottoms js
    else
        head bottoms:match tops (tail bottoms) js

count x y = length (filter x y)

isFailed :: Job -> Bool
isFailed (Job { status = Fail _ }) = True
isFailed _ = False

color :: Job -> (Double, Double, Double, Double)
color (Job { status = Idle }) = (0, 1, 0, 1)
color (Job { status = Working }) = (0, 1, 1, 1)
color (Job { status = Fail _ }) = (1, 0, 0, 1)

errorText :: Job -> [String]
errorText (Job { status = Fail s }) = lines s
errorText _ = []

data Rect = Rect Double Double Double Double

shrink :: Double -> Rect -> Rect
shrink by (Rect x y w h) = Rect (x+by) (y+by) (w-2*by) (h-2*by)

splitVertical :: Rect -> Int -> [Rect]
splitVertical (Rect x y w h) n = map calcNew [1..n]
    where
        calcNew n = Rect x (y+(fromIntegral n-1)*newH) w newH
        newH = h / fromIntegral n

splitHorizontal :: Rect -> Int -> [Rect]
splitHorizontal (Rect x y w h) n = map calcNew [1..n]
    where
        calcNew n = Rect (x+(fromIntegral n -1)*newW) y newW h
        newW = w / fromIntegral n

divideVertical :: Rect -> Double -> (Rect, Rect)
divideVertical (Rect x y w h) percent = (Rect x y w splitH, Rect x splitH w (h - splitH))
    where splitH = percent * h
