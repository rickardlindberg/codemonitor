module Render where

import Control.Monad
import Graphics.Rendering.Cairo hiding (status)
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
        moveTo 10 (y+20)
        showText (name job)

        let errors = errorText job
        let ys = map (\x -> fromIntegral x*10 + y + 20) [1..length errors]
        forM_ (zip errors ys) $ \(e, y) -> do
            moveTo 20 y
            showText e
        return ()

color :: Job -> (Double, Double, Double, Double)
color (Job { status = Idle }) = (0, 1, 0, 1)
color (Job { status = Working }) = (0, 1, 1, 1)
color (Job { status = Fail _ }) = (1, 0, 0, 1)

errorText :: Job -> [String]
errorText (Job { status = Fail s }) = lines s
errorText _ = []

data Rect = Rect Double Double Double Double

splitVertical :: Rect -> Int -> [Rect]
splitVertical (Rect x y w h) n = map calcNew [1..n]
    where
        calcNew n = Rect x ((fromIntegral(n)-1)*newH) w newH
        newH = h / fromIntegral(n)
