module Layout where

import Job

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
