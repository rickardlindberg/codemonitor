module Render.Graphics where

import Control.Arrow
import Control.Monad
import Graphics.Rendering.Cairo hiding (status, Status)
import Job.Types
import Monitor
import Render.Layout
import Render.Rect

type Color = (Double, Double, Double, Double)

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
renderMonitor (monitor@StatusCodeMonitor {}, rect) =
    renderDocumentBox
        rect
        (statusToBgColor (mSecondsInState monitor) (mJobStatus monitor))
        (mJobName monitor)
        (additionalLines (mJobStatus monitor) (mOutput monitor))
renderMonitor (monitor@StdoutMonitor {}, rect) =
    renderDocumentBox
        rect
        (1, 0, 1, 1)
        (mJobName monitor)
        (lines (mOutput monitor))

renderDocumentBox :: Rect -> Color -> String -> [String] -> Render ()
renderDocumentBox rect color heading body = do
    renderRoundedRectangle rect color
    renderDocument (shrink boxRadius rect) heading body

renderRoundedRectangle :: Rect -> Color -> Render ()
renderRoundedRectangle rect (r, g, b, a) = do
    setSourceRGBA r g b a
    roundRectPath rect
    fillPreserve
    setSourceRGBA 0 0 0 0.4
    setLineWidth 1.5
    stroke

roundRectPath :: Rect -> Render ()
roundRectPath (Rect x y w h) = do
    newPath
    arc (x+w-boxRadius) (y+  boxRadius) boxRadius ((-90) * pi/180) (0   * pi/180)
    arc (x+w-boxRadius) (y+h-boxRadius) boxRadius (0     * pi/180) (90  * pi/180)
    arc (x+  boxRadius) (y+h-boxRadius) boxRadius (90    * pi/180) (180 * pi/180)
    arc (x+  boxRadius) (y+  boxRadius) boxRadius (180   * pi/180) (270 * pi/180)
    closePath

renderDocument :: Rect -> String -> [String] -> Render ()
renderDocument rect@(Rect x y w h) heading body =
    withClipRegion rect $ do
        -- Title
        selectFontFace fontName FontSlantNormal FontWeightBold
        setFontSize fontSize
        setSourceRGBA 0 0 0 0.7
        ex <- textExtents heading
        moveTo (x - textExtentsXbearing ex) (y - textExtentsYbearing ex)
        showText heading
        -- Additional text
        let th = textExtentsHeight ex
        let textRect@(Rect x2 y2 w2 h2) = Rect (x+boxRadius) (y+2*th) (w-boxRadius) (h-2*th)
        renderMultilineText textRect body

renderMultilineText :: Rect -> [String] -> Render ()
renderMultilineText rect@(Rect x y w h) text = withClipRegion rect $ do
    selectFontFace fontName FontSlantNormal FontWeightNormal
    setFontSize    fontSize
    setSourceRGBA  0 0 0 0.8
    fontHeight <- fmap fontExtentsHeight fontExtents
    let totalHeight = fromIntegral (length text) * fontHeight
    let startY = if totalHeight <= h
                     then y
                     else y - (totalHeight - h)
    let ys = map (\i -> fromIntegral i*fontHeight + startY) [1..length text]
    forM_ (zip text ys) $ \(line, y) -> do
        moveTo x y
        showText line
    return ()

withClipRegion :: Rect -> Render () -> Render ()
withClipRegion (Rect x y w h) r = do
    save
    rectangle x y w h
    clip
    r
    restore

statusToBgColor :: Double -> Status -> Color
statusToBgColor t Idle    = (121/255, 245/255, 0, 1)
statusToBgColor t Working = (0, 204/255, 245/255, circularMovement 1 0.5 2 t)
statusToBgColor t Fail    = (245/255, 36/255, 0, circularMovement 1 0.7 0.5 t)

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

additionalLines :: Status -> String -> [String]
additionalLines Fail s = lines s
additionalLines _    _ = []
