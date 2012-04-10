module Rect where

data Rect = Rect Double Double Double Double

shrink :: Double -> Rect -> Rect
shrink by (Rect x y w h) = Rect (x+by) (y+by) (w-2*by) (h-2*by)

divideVertical :: Rect -> Double -> (Rect, Rect)
divideVertical (Rect x y w h) percent = (Rect x y w splitH, Rect x splitH w (h - splitH))
    where splitH = percent * h

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
