module Render where

import Control.Monad
import Graphics.Rendering.Cairo
import Job

renderScreen :: [Job] -> Double -> Double -> Render ()
renderScreen jobs w h = do
    setSourceRGB 1 1 1
    paint

    forM_ (zip jobs [1..length jobs]) $ \(job, i) -> do
        let y = fromIntegral(10 + 10 * i)
        let (r, g, b, a) = color job
        setSourceRGBA r g b a
        moveTo 10 y
        showText (name job)

color :: Job -> (Double, Double, Double, Double)
color (Job _ _ Nothing) = (0, 1, 0, 1)
color (Job _ _ (Just _)) = (0, 1, 1, 1)
