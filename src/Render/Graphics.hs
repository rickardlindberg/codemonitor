module Render.Graphics where

import Control.Arrow
import Control.Monad
import Data.List.Utils
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
fontSize = 14.0
backgroundColor = (1, 1, 0.6, 1)
runningColor = (0, 204/255, 245/255, 1)
successColor = (121/255, 245/255, 0, 1)
failureColor = (255/255, 173/255, 173/255, 1)
monitorColor = (1, 0, 1, 1)

renderScreen :: [Monitor] -> Double -> Double -> Render ()
renderScreen monitors w h = do
    renderBackground
    renderMonitors monitors (shrink outerSpace $ Rect 0 0 w h)

renderBackground :: Render ()
renderBackground = do
    let (r, g, b, a) = backgroundColor
    setSourceRGBA r g b a
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
        (statusToBgColor (mJobStatus monitor))
        (mJobName monitor)
        (additionalLines (mJobStatus monitor) (mOutput monitor))
renderMonitor (monitor@StdoutMonitor {}, rect) =
    renderDocumentBox
        rect
        monitorColor
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
        setSourceRGBA 0 0 0 0.8
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
    let fontOutsideClipCompensation = 2
    let ys = map (\i -> fromIntegral i*fontHeight + startY - fontOutsideClipCompensation) [1..length text]
    forM_ (zip text ys) $ \(line, y) -> do
        moveTo x y
        showText $ replace "\t" "    " line
    return ()

withClipRegion :: Rect -> Render () -> Render ()
withClipRegion (Rect x y w h) r = do
    save
    rectangle x y w h
    clip
    r
    restore

statusToBgColor :: Status -> Color
statusToBgColor Idle    = successColor
statusToBgColor Working = runningColor
statusToBgColor Fail    = failureColor

additionalLines :: Status -> String -> [String]
additionalLines Fail s = lines s
additionalLines _    _ = []
