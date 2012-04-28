module Render where

import Control.Arrow
import Control.Monad
import Graphics.Rendering.Cairo hiding (status, Status)
import Job
import Layout
import Monitor
import Rect

boxRadius = 10.0
innerSpace = 2
outerSpace = innerSpace/2
fontName = "Monospace"
fontSize = 11.0

renderScreen :: [Monitor] -> Double -> Double -> Render ()
renderScreen monitors w h = do
    renderBackground
    renderMonitors monitors (shrink outerSpace $ Rect 0 0 w h)

renderBackground :: Render ()
renderBackground = do
    setSourceRGB 1 1 0.6
    paint

renderMonitors :: [Monitor] -> Rect -> Render ()
renderMonitors monitors rect = do
    let mr       = findRects rect monitors
    let mrShrunk = map (second $ shrink innerSpace) mr
    forM_ mrShrunk renderMonitor

renderMonitor :: (Monitor, Rect) -> Render ()
renderMonitor (monitor@JobMonitor {}, rect@(Rect x y w h)) = do
    -- Background
    let (r, g, b, a) = statusToBgColor (mSecondsInState monitor) (mJobStatus monitor)
    setSourceRGBA r g b a
    roundRectPath rect
    fillPreserve
    setSourceRGBA 0 0 0 0.4
    setLineWidth 1.5
    stroke
    -- Title
    selectFontFace fontName FontSlantNormal FontWeightBold
    setFontSize fontSize
    setSourceRGBA 0 0 0 0.7
    ex <- textExtents (mJobName monitor)
    moveTo (x + boxRadius - textExtentsXbearing ex) (y + boxRadius - textExtentsYbearing ex)
    showText (mJobName monitor)
    -- Additional text
    selectFontFace fontName FontSlantNormal FontWeightNormal
    setFontSize fontSize
    setSourceRGBA 0 0 0 0.8
    moveTo (x + 10) (y+20)
    ex2 <- fontExtents
    let errors = additionalLines (mJobStatus monitor)
    let ys = map (\x -> fromIntegral x*fontExtentsHeight ex2 + y + boxRadius + 2*textExtentsHeight ex) [1..length errors]
    forM_ (zip errors ys) $ \(e, y) -> do
        moveTo (x + boxRadius + boxRadius) y
        showText e
    return ()

statusToBgColor :: Double -> Status -> (Double, Double, Double, Double)
statusToBgColor t Idle     = (121/255, 245/255, 0, 1)
statusToBgColor t Working  = (0, 204/255, 245/255, circularMovement 1 0.5 2 t)
statusToBgColor t (Fail _) = (245/255, 36/255, 0, circularMovement 1 0.7 0.5 t)

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

roundRectPath :: Rect -> Render ()
roundRectPath (Rect x y w h) = do
    newPath
    arc (x+w-boxRadius) (y+  boxRadius) boxRadius ((-90) * pi/180) (0   * pi/180)
    arc (x+w-boxRadius) (y+h-boxRadius) boxRadius (0     * pi/180) (90  * pi/180)
    arc (x+  boxRadius) (y+h-boxRadius) boxRadius (90    * pi/180) (180 * pi/180)
    arc (x+  boxRadius) (y+  boxRadius) boxRadius (180   * pi/180) (270 * pi/180)
    closePath

additionalLines :: Status -> [String]
additionalLines (Fail s) = lines s
additionalLines _        = []
