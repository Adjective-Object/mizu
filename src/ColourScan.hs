module ColourScan where

import Colours
import ColourMatch
import Data.Char (isHexDigit)
import Data.List (minimumBy, nub)
import Data.Map.Strict (Map, fromList, toList, mapKeys)
import qualified Data.Map.Strict as Map (map)

import System.IO
import System.FilePath.Canonical (CanonicalFilePath, canonicalFilePath)
import Control.Monad (when, sequence)

colourFileFold :: String -> (a -> String -> a) -> a -> Handle -> IO(a)
colourFileFold buildColour foldfn out filehandle = do
    isEnd <- hIsEOF filehandle
    currentChar <- if not isEnd
        then hGetChar filehandle
        else return '\0'

    if isEnd
        then return out
        else let
            currentColour = currentChar : buildColour
            hexColour     = Hex (reverse currentColour)
            prevColour    = Hex (reverse buildColour)
            (nextOut, nextColour)
                | currentChar == '#' -- open a new capture on '#''
                    = (out, [currentChar])

                -- store capture if it's 6 digits long
                | length currentColour == 7 -- close a capture
                    = if isHexDigit currentChar
                        then (foldfn hexColour out, [])
                        else (out,                  [])

                -- store capture if it's 3 digits long and ended
                | length buildColour == 4
                    && not (isHexDigit currentChar)
                    = (foldfn prevColour out, [])

                -- add to capture
                | 0 < length currentColour
                    && length currentColour < 7
                    && isHexDigit currentChar
                    = (out, currentColour)

                -- not in a capture, or capture was broken
                | otherwise
                    = (out, [])

            in colourFileFold foldfn nextColour out nextColour filehandle

scanForColours = colourFileFold "" (:) []

findColours :: String -> IO[HexColour]
findColours path = do
    fileHandle <- openFile path ReadMode
    scanForColours [] "" fileHandle




-- create a list of IO actions from a string of files to parse
-- as well as the colour map to match against
matchColoursOnFiles :: Map String String -> [CanonicalFilePath] -> IO()
matchColoursOnFiles hexTextMap paths =
    let stringPaths = map canonicalFilePath paths
        coloursActions :: IO[[HexColour]]
        coloursActions = mapM findColours stringPaths

    in do
        -- find the colours in the files
        hexColoursByFile <- coloursActions
        let hexColours = concat hexColoursByFile
            labColours = map convert hexColours

        print hexColours

        let hexMap =  Map.map Hex hexTextMap
            labMap =  Map.map convert hexMap :: (Map String LABColour)
            colourPairs = zip hexColours labColours

        let colourMatches = matchColours labMap colourPairs

        -- substitute back into the source file
        print colourMatches

