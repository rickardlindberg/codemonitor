module Render.Layout where

import GHC.Exts
import Job.Types
import Monitor
import Render.Rect

data AttentionLevel = High | Medium | Low deriving (Eq, Ord)

findRects :: Rect -> [Monitor] -> [(Monitor, Rect)]
findRects originalRect monitors = zip orderedMonitors rectangles
    where
        orderedMonitors = orderMonitors monitors
        rectangles
            | orderedMonitors == []
                = []
            | (hasHighest || hasMedium) && numMonitors == 1
                = [originalRect]
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
        numMonitors = length orderedMonitors
        hasHighest  = countAttentionLevels High   orderedMonitors > 0
        numMedium   = countAttentionLevels Medium orderedMonitors
        hasMedium   = numMedium > 0

orderMonitors :: [Monitor] -> [Monitor]
orderMonitors = sortWith attentionLevel

attentionLevel :: Monitor -> AttentionLevel
attentionLevel (StatusCodeMonitor _ _ _ Fail _) = High
attentionLevel (StdoutMonitor _ _ _ _)          = Medium
attentionLevel _                                = Low

countAttentionLevels :: AttentionLevel -> [Monitor] -> Int
countAttentionLevels level = length . filterAttentionLevel level

filterAttentionLevel :: AttentionLevel -> [Monitor] -> [Monitor]
filterAttentionLevel level = filter ((==level) . attentionLevel)
