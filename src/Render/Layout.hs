module Render.Layout where

import Data.Maybe
import Job.Types
import Monitor
import qualified Data.Map as M
import Render.Rect

data RectType = Small
              | Large
              deriving (Eq, Ord)

type RectMap = M.Map RectType [Rect]

findRects :: Rect -> [Monitor] -> [(Monitor, Rect)]
findRects originalRect monitors = match monitors rectMap
    where
        rectMap           = rectsForTypes originalRect monitors
        match []     _    = []
        match (m:ms) mmap = let (rect, restMap) = rectMapPop (rectType m) mmap
                            in (m, rect):match ms restMap

rectsForTypes :: Rect -> [Monitor] -> RectMap
rectsForTypes originalRect monitors =
    let types     = map rectType monitors
        numSmalle = length $ filter (==Small) types
        numLarge  = length $ filter (==Large) types
        smallArea = if numLarge == 0
                        then snd $ divideVertical originalRect 0.7
                        else snd $ divideVertical originalRect 0.9
        largeArea = if numSmalle == 0
                        then originalRect
                        else fst (divideVertical originalRect 0.9)
    in rectMapCreate
           [ (Small, splitHorizontal smallArea numSmalle)
           , (Large, splitVertical largeArea numLarge)
           ]

rectType :: Monitor -> RectType
rectType (StatusCodeMonitor { mJobStatus = Fail }) = Large
rectType _                                         = Small

rectMapPop :: RectType -> RectMap -> (Rect, RectMap)
rectMapPop t m = let rects     = fromJust $ M.lookup t m
                     firstRect = head rects
                     restRects = tail rects
                 in (firstRect, M.insert t restRects m)

rectMapCreate :: [(RectType, [Rect])] -> RectMap
rectMapCreate = M.fromList
