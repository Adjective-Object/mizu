module ColourScan where

import Colours
import Data.Char (isHexDigit)
import Data.List (minimumBy, nub)
import Data.Map (Map, fromList, toList)

import System.IO
import System.FilePath.Canonical (CanonicalFilePath, canonicalFilePath)
import Control.Monad (when, sequence)


scanForColours :: [HexColour] -> String -> Handle -> IO[HexColour]
scanForColours knownColours buildColour filehandle = do
    isEnd <- hIsEOF filehandle
    currentChar <- if not isEnd
        then hGetChar filehandle
        else return '\0'

    if isEnd
        then return knownColours
        else let
            currentColour = currentChar : buildColour
            hexColour     = Hex (reverse currentColour)
            prevColour    = Hex (reverse buildColour)
            (nextKnown, nextColour)
                | currentChar == '#' -- open a new capture on '#''
                    = (knownColours, [currentChar])

                -- store capture if it's 6 digits long
                | length currentColour == 7 -- close a capture
                    = if isHexDigit currentChar
                        then (hexColour : knownColours, [])
                        else (knownColours,             [])

                -- store capture if it's 3 digits long and ended
                | length buildColour == 4
                    && not (isHexDigit currentChar)
                    = (prevColour : knownColours, [])

                -- add to capture
                | 0 < length currentColour
                    && length currentColour < 7
                    && isHexDigit currentChar
                    = (knownColours, currentColour)

                -- not in a capture, or capture was broken
                | otherwise
                    = (knownColours, [])

            in scanForColours nextKnown nextColour filehandle

findColours :: String -> IO[HexColour]
findColours path = do
    fileHandle <- openFile path ReadMode
    scanForColours [] "" fileHandle



data ColourMatch = ColourMatch
    { matchName         :: String
    , matchDifference   :: Double -- L channel distance (thisColour - matchColour)
    , outString         :: String
    } deriving Show

matchColours :: [(LABColour, String)] -> [LABColour] -> [ColourMatch]
matchColours labPairs foundColours =
    let lDiff (LAB l1 _ _) (LAB l2 _ _, name) = ColourMatch
            { matchName = name
            , matchDifference = l1 - l2
            , outString = generateOutString name (l1 - l2) }
        distances c = map (lDiff c) labPairs
        closest c = minimumBy
            (\ a b -> compare
                (abs $ matchDifference a)
                (abs $ matchDifference b))
            $ distances c
    in map closest foundColours

generateOutString :: String -> Double -> String
generateOutString colourName lumDiff =
    "{[ CIELumDiff( " ++ colourName ++ "_Lab, " ++ show lumDiff ++ ") ]}"



-- create a list of IO actions from a string of files to parse
-- as well as the colour map to match against
matchColoursOnFiles :: Map String String -> [CanonicalFilePath] -> IO()
matchColoursOnFiles colourMap paths =
    let stringPaths = map canonicalFilePath paths
        coloursActions :: IO[[HexColour]]
        coloursActions = mapM findColours stringPaths

    in do
        -- find the colours in the files
        hexColoursByFile <- coloursActions
        let hexColours = nub $ concat hexColoursByFile
        print hexColours

        let labColours :: [LABColour]
            labColours = map convert hexColours

        -- match them to the colours given in the cmap
        let reverseLabPairs =
                map (\ (name, cstr) -> (convert (Hex cstr) :: LABColour, name))
                    (toList colourMap)
            colourMatches = matchColours reverseLabPairs labColours
            matchMap = fromList $ zip hexColours colourMatches

        -- substitute back into the source file
        print matchMap

