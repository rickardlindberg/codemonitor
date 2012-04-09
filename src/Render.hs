module Render where

import Control.Monad
import Graphics.Rendering.Cairo
import Job

renderScreen :: [Job] -> Double -> Double -> Render ()
renderScreen jobs w h = do
    setSourceRGB 1 1 1
    paint

    forM_ (zip jobs (splitVertical (Rect 0 0 w h) (length jobs))) $ \(job, Rect x y w h) -> do
        newPath
        let (r, g, b, a) = color job
        setSourceRGBA r g b a
        rectangle (x+5) (y+5) (w-10) (h-10)
        fill

        setSourceRGBA 0 0 0 1
        moveTo 10 (y+10)
        showText (name job)

color :: Job -> (Double, Double, Double, Double)
color (Job { process = Nothing }) = (0, 1, 0, 1)
color (Job { process = Just _ }) = (0, 1, 1, 1)

data Rect = Rect Double Double Double Double

splitVertical :: Rect -> Int -> [Rect]
splitVertical (Rect x y w h) n = map calcNew [1..n]
    where
        calcNew n = Rect x ((fromIntegral(n)-1)*newH) w newH
        newH = h / fromIntegral(n)
