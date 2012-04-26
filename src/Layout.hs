module Layout where

import Job
import Monitor
import Rect

findRects :: Rect -> [Monitor] -> [Rect]
findRects rect monitors =
    if any isFailed monitors then
        let (top, bottom) = divideVertical rect 0.9
            tops = splitVertical top (count isFailed monitors)
            bottoms = splitHorizontal bottom (count (not . isFailed) monitors)
        in match tops bottoms monitors
    else
        splitVertical rect (length monitors)

match [] [] [] = []
match tops bottoms (j:js) =
    if isFailed j then
        head tops:match (tail tops) bottoms js
    else
        head bottoms:match tops (tail bottoms) js

count x y = length (filter x y)

isFailed :: Monitor -> Bool
isFailed (JobMonitor _ _ (Fail _)) = True
isFailed _                         = False

