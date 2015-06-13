module ColourMatch where
import Colours

import Data.List(sort, minimumBy)
import Data.Map.Strict (Map, fromList, toList, keys)

first :: (a, b) -> a
first (x, _) = x

second :: (a, b) -> b
second (_, x) = x

median :: (Ord a) => [a] -> a
median xs = sort xs !! (length xs `quot` 2)

safeMedian :: (Ord a) => a -> [a] -> a
safeMedian fallback xs
    | not (null xs) = median xs
    | otherwise     = fallback

data ColourMatch = ColourMatch
    { matchName :: String
    , lumDiff :: Double
    } deriving Show

matchToBins :: [LABColour] ->
    [(HexColour, LABColour)] ->
    [[(HexColour, LABColour)]]
matchToBins templateColours dataColours =
    let templateLums = map lum templateColours
        dataLABColours = map second dataColours
        dataLums = map lum dataLABColours
        closestIndex someColour =
            first . minimumBy 
                (\(_,x) (_,y) -> compare x y)
                $ zip [0..] (map (labDistanceAB someColour) templateColours)
        dataClosestIndecies :: [Int]
        dataClosestIndecies = map closestIndex dataLABColours
        emptyLists = replicate (length templateColours) []
        appendByIndex :: [[a]] -> (Int, a) -> [[a]]
        appendByIndex lists (index, value) =
            [ if index == i
                then value : (lists !! i)
                else ls
                | (i, ls) <- zip [0..] lists ]
        coloursByClosest :: [(Int, (HexColour, LABColour))]  
        coloursByClosest = zip dataClosestIndecies dataColours
    in foldl appendByIndex emptyLists coloursByClosest

matchColours :: Map String LABColour ->
    [(HexColour, LABColour)] ->
    Map HexColour ColourMatch
matchColours labMap foundColours =
    let names = map first $ toList labMap
        colours = map second $ toList labMap
        colourBins :: [[(HexColour, LABColour)]]
        colourBins = matchToBins colours foundColours
        binMedianLums :: [Double]
        binMedianLums = map (safeMedian 0 . map (lum . second)) colourBins
        hexVariationsFromMedian :: [[(HexColour, Double)]]
        hexVariationsFromMedian = map
            (\ (med, colours) ->
                map (\ (hex, lab) -> (hex, lum lab - med))
                colours )
            $zip binMedianLums colourBins

        makeMatches (name, matchdata) =
            map (\ (hex, lumdiff) ->
                (hex, ColourMatch
                    { matchName = name
                    , lumDiff = lumdiff }))
            matchdata

    in fromList . concatMap makeMatches
        $ zip names hexVariationsFromMedian
