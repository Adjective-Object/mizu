module Colours where
import Data.Map (Map)
import Data.List (foldl', reverse)
import Data.Char (isHexDigit)
import System.IO
import System.FilePath.Canonical (CanonicalFilePath, canonicalFilePath)
import Control.Monad (when, sequence)

data Colour = Hex String 
    | RGB Int Int Int 
    | LAB Int Int Int
    deriving Show


scanForColours :: [Colour] -> String -> Handle -> IO[Colour]
scanForColours knownColours buildColour filehandle = do
    isEnd <- hIsEOF filehandle
    currentChar <- if not isEnd
        then hGetChar filehandle
        else return '\0'

    --print knownColours ++ " "
    --            ++ show buildColour ++ " "
    --            ++ show currentChar

    if isEnd
        then return knownColours
        else let
            currentColour = currentChar : buildColour
            hexColour = Hex (reverse currentColour)
            (nextKnown, nextColour)
                | currentChar == '#' -- open a new capture on '#''
                    = (knownColours, [currentChar])

                -- store capture if it's 6 digits long
                | length currentColour == 6 -- close a capture
                    = if isHexDigit currentChar
                        then (hexColour : knownColours, [])
                        else (knownColours,             [])

                -- store capture if it's 3 digits long and ended
                | length buildColour == 4
                    && not (isHexDigit currentChar)
                    = (hexColour : knownColours, [])

                -- add to capture
                | 0 < length currentColour
                    && length currentColour < 7
                    && isHexDigit currentChar
                    = (knownColours, currentColour)

                -- not in a capture, or capture was broken
                | otherwise
                    = (knownColours, [])

            in scanForColours nextKnown nextColour filehandle

findColours :: String -> IO[Colour]
findColours path = do
    fileHandle <- openFile path ReadMode
    scanForColours [] "" fileHandle

-- create a list of IO actions from a string of files to parse
-- as well as the colour map to match against
matchColoursOnFiles :: Map String String -> [CanonicalFilePath] -> IO()
matchColoursOnFiles colourMap paths =
    let stringPaths = map canonicalFilePath paths
        colours_actions :: IO[[Colour]]
        colours_actions = mapM findColours stringPaths

    in do
        colours <- colours_actions

        print colours


