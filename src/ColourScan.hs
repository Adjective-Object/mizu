module ColourScan where

import Colours
import ColourMatch
import qualified Args (MIZU_CONF(..))
import Data.Char (isHexDigit)
import Data.List (minimumBy, nub)
import Data.Map.Strict (Map, fromList, toList, mapKeys, (!))
import qualified Data.Map.Strict as Map (map)
import System.FilePath (takeFileName, replaceFileName)

import System.IO
import System.FilePath.Canonical (CanonicalFilePath, canonicalFilePath)
import Control.Monad (when, sequence)

import Debug.Trace

colourFileFold :: String -> (Bool -> String -> a -> a) -> a -> Handle -> IO a
colourFileFold buildColour foldfn out filehandle = do
    isEnd <- hIsEOF filehandle
    currentChar <- if not isEnd
        then hGetChar filehandle
        else return '\0'

    if isEnd
        then return (foldfn False (reverse buildColour) out)
        else let
            currentColour = currentChar : buildColour
            prevColour    = reverse buildColour
            (nextOut, nextColour)
                | currentChar == '#' -- open a new capture on '#''
                    = (out, [currentChar])

                -- store capture if it's 6 digits long
                | length currentColour == 7 -- close a capture
                    = if isHexDigit currentChar
                        then (foldfn True (reverse currentColour) out, [])
                        else (out,                      [])

                -- store capture if it's 3 digits long and ended
                | length buildColour == 4
                    && not (isHexDigit currentChar)
                    = (foldfn True prevColour out, [currentChar])

                -- add to capture
                | 0 < length buildColour
                    && length buildColour < 7
                    && isHexDigit currentChar
                    = (out, currentColour)

                -- not in a capture, or capture was broken
                | otherwise
                    = (foldfn False (reverse currentColour) out, [])

            in colourFileFold nextColour foldfn nextOut filehandle

keepIfColour :: Bool -> String -> [HexColour] -> [HexColour]
keepIfColour isColour a b =
    if isColour
        then ((:) . Hex) a b
        else b

findColoursInFile :: Handle -> IO[HexColour]
findColoursInFile = colourFileFold "" keepIfColour []

findColours :: String -> IO[HexColour]
findColours path = do
    fileHandle <- openFile path ReadMode
    findColoursInFile fileHandle

replaceColour :: Map String String
    -> Handle
    -> Bool
    -> String
    -> [IO()] -> [IO()]
replaceColour stringMap writeHandle isColour str actions =
    actions ++
        [hPutStr writeHandle $
            if isColour
                then (stringMap ! str)
                else str]

translateFileTo :: String -> Map String String -> FilePath -> FilePath -> IO[IO()]
translateFileTo header cMap inPath outPath = do
    readHandle  <- openFile inPath ReadMode
    writeHandle <- openFile outPath WriteMode
    writeActions<- colourFileFold ""
        (replaceColour cMap writeHandle) [] readHandle

    putStrLn (inPath ++ " -> " ++ outPath)
    hPutStr writeHandle header

    return $ writeActions ++ [hClose writeHandle]



translateFile :: Args.MIZU_CONF -> Map String String -> FilePath -> IO[IO()]
translateFile conf cMap path = translateFileTo header cMap path outPath
    where 
        outPath = case Args.outputDir conf of
                    Just outDir -> outDir ++ (takeFileName path) ++ ".xgcm"
                    Nothing     -> path ++ ".xgcm"

        header = if Args.destinationFixed conf
                    then "{[~ xgcm_output_path(\"" ++ escapeSpaces path ++ "\") ]}"
                    else ""

        escapeSpaces ('\\' : ' ' : rest) = '\\' : ' '  : escapeSpaces rest
        escapeSpaces ( x   : ' ' : rest) = x    : '\\' : ' ' : escapeSpaces rest
        escapeSpaces ( x   : rest)       = x    : escapeSpaces rest
        escapeSpaces ""                  = ""



unpackAndDoSequence :: IO[IO a] -> IO()
unpackAndDoSequence x = do ys <- x
                           doSequence ys

doSequence :: [IO a] -> IO ()
doSequence [] = return ()
doSequence (x:xs) = do x
                       doSequence xs


-- create a list of IO actions from a string of files to parse
-- as well as the colour map to match against
matchColoursOnFiles :: Args.MIZU_CONF -> IO()
matchColoursOnFiles conf =
    let hexTextMap  = Args.colourMap conf
        paths       = Args.paths conf
        colourTranslator = Args.translator conf

        coloursActions :: IO[[HexColour]]
        coloursActions = mapM findColours paths

    in do
        -- find the colours in the files
        hexColoursByFile <- coloursActions
        let hexColours = concat hexColoursByFile
            labColours = map convert hexColours

        let hexMap =  Map.map Hex hexTextMap
            labMap =  Map.map convert hexMap :: (Map String LABColour)
            colourPairs = zip hexColours labColours

        let colourMatches = matchColours labMap colourPairs
            stringMap :: Map String String
            stringMap = Map.map colourTranslator
                $ mapKeys (\ (Hex x) -> x) colourMatches

        let writeActions :: [IO()]
            writeActions = map
                (unpackAndDoSequence . translateFile conf stringMap)
                paths

        doSequence writeActions
