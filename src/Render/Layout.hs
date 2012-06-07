module Render.Layout where

import Data.List
import Job.Types
import Monitor
import Render.Rect

highLevel   = 100
mediumLevel = 50
lowLevel    = 0

findRects :: Rect -> [Monitor] -> [(Monitor, Rect)]
findRects originalRect monitors = zip orderedMonitors rectangles
    where
        orderedMonitors = orderMonitors monitors
        rectangles
            | orderedMonitors == []
                = []
            | hasHighest
                = let (topArea, bottomArea) = divideVertical originalRect 0.9
                      tops    = splitVertical topArea 1
                      bottoms = splitHorizontal bottomArea (numMonitors - 1)
                  in tops ++ bottoms
            | hasMedium
                = let (topArea, bottomArea) = divideVertical originalRect 0.9
                      tops    = splitVertical topArea numMedium
                      bottoms = splitHorizontal bottomArea (numMonitors - numMedium)
                  in tops ++ bottoms
            | otherwise
                = let (_, bottomArea) = divideVertical originalRect 0.7
                      bottoms = splitHorizontal bottomArea numMonitors
                  in bottoms
        numMonitors       = length orderedMonitors
        hasHighest        = countLevels highLevel > 0
        numMedium         = countLevels mediumLevel
        hasMedium         = numMedium > 0
        countLevels level = length $ filter ((==level) . attentionLevel) orderedMonitors

orderMonitors :: [Monitor] -> [Monitor]
orderMonitors monitors = sortBy cmp monitors
    where
        cmp a b
            | attentionLevel a > attentionLevel b = LT
            | attentionLevel a < attentionLevel b = GT
            | otherwise                           = EQ

attentionLevel :: Monitor -> Int
attentionLevel (StatusCodeMonitor _ _ _ Fail _) = highLevel
attentionLevel (StdoutMonitor _ _ _ _)          = mediumLevel
attentionLevel _                                = lowLevel
