module Layout where

import Job
import Rect

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
