module Main where

import System.IO (stdin)
import System.Exit (exitWith, exitSuccess, ExitCode(..))
import System.FilePath.Canonical
import System.Environment (getProgName, getArgs)
import System.Path.Glob(glob)
import System.Console.GetOpt (
    getOpt',
    OptDescr(..),
    ArgDescr(..),
    ArgOrder(Permute))

import Data.List (foldl', intercalate, repeat, intersect)
import Data.Map (Map, fromList)
import Control.Monad (when, sequence)
import Debug.Trace (trace)

import Data.List.Split(splitOn)
import Data.Aeson (decode)
import Data.ByteString.Char8(pack)
import Data.ByteString.Lazy as BSL (fromChunks, readFile)

import ColourScan (matchColoursOnFiles)

-- cast a value to string 
traceVal x = trace (show x) x

-- split path on image decoding error
either' switch fail succeed = either fail succeed switch

-- shorthand for exiting with an error msg
exitWithError err = do
    putStrLn err
    exitWith (ExitFailure 1)



data MaybeFlag = Error String | Flag FLAG

instance Show (IO a) where
    show _ = "<ioref>"

instance Eq (IO a) where
    (==) _ _ = False

-- options
data FLAG = HELP
    | DESTINATION_FIXED
    | OUTPUT_DIRECTORY String
    | COLOUR_MAPPING  (IO(Map String String))
    deriving (Show, Eq)

(+\) a b = a ++ "\n" ++ b
(+\\) a b = a ++ "\n    " ++ b

-- help text
helpText :: String -> String
helpText progName =
    "A tool for generating xgcm templates from text files by" ++
    "matching hex colours (#XXXXXX or #XXX) to"+\
    "a provided colour set" +\
    "usage: " ++ progName ++ " [options] [files...]" +\
    "[options] one of" +\\
        intercalate "\n\n    " optionStrings

optionStrings :: [String]
optionStrings =
    let flagDescPairs =
            map 
                (\ (Option shorts longs _ desc) ->
                    let flagNames = map (\c -> ['-',c]) shorts ++ 
                                    map (\l -> "--" ++ l) longs
                    in (intercalate ", " flagNames, 
                        splitOn "\n" desc)) 
                options
        maxFlagStringLength = 
            maximum $ map 
                (\ (flagStr, _) -> length flagStr)
                flagDescPairs
        leftColumnPadding = "\n" ++ replicate (12 + maxFlagStringLength) ' '
    in map
        (\ (flagStr, descLines) ->
            flagStr
            ++ replicate (8 + maxFlagStringLength - length flagStr) ' '
            ++ intercalate leftColumnPadding descLines)
        flagDescPairs



-- opt definitions
helpOpt =
    Option "h" ["help"]
        (NoArg (Flag HELP))
        "display helptext and exit"

destdirOpt =
    Option "d" ["template-destination"]
        (NoArg (Flag DESTINATION_FIXED))
        ("When specified, the generated templates will point to the"+\
        "location of the original files on in the filesystem"+\
        "by absolute path.")

outdirOpt =
    Option "o" ["output-directory"]
        (ReqArg mapping "PATH")
        ("Output directory (place the templates will be placed)."+\
        "If left unspecified, templates will be left next to" +\
        "source files")
    where mapping str = Flag (OUTPUT_DIRECTORY str)

colourMapOpt =
    Option "c" ["colours", "colors"]
        (ReqArg mapping "COLOUR_MAPPING")
        ("a valid JSON string of the mapping between colour names to"+\
         "hex codes. Example:"+\\
             "{ \"red\" :  \"#FF0000\""+\\
             ", \"green\": \"#00FF00\""+\\
             ", \"blue\":  \"#0000FF\" }"+\
         "If it is not a valid JSON string, it will be treated as a"+\
         "file path, and  the apprpriate file will be parsed as json")
    where mapping str = Flag (COLOUR_MAPPING $ loadColour str)

defaultColourMap = fromList 
    [ ("black", "#000000")
    , ("white", "#FFFFFF")
    , ("green", "#00FF00")
    , ("blue",  "#0000FF")
    , ("red",   "#FF0000")
    , ("yellow","#FFFF00")
    , ("cyan",  "#00FFFF")
    , ("purple","#FF00FF")
    ]

options :: [OptDescr MaybeFlag]
options =
    [ helpOpt
    , outdirOpt
    , destdirOpt
    , colourMapOpt
    ]

getOpt :: [String] -> Either String ([FLAG], [String])
getOpt argv =
    let (flags, nonopts, unknownopts, errs) = getOpt' Permute options argv
        flagErrors = filter
            (\ f -> case f of
                Error _ -> True
                _       -> False) flags

    in if not $ null flagErrors -- if flagErrors is empty
        then let flagErrorMessages = map (\ (Error s) -> s) flagErrors
            in Left ("Error in parsing flags:\n\t"
                        ++ intercalate "\n\t" flagErrorMessages)
        else if not $ null errs
            then Left ("Error parsing argv\n" ++ foldl' (++) "" errs)
            else let realFlags = map (\ (Flag f) -> f) flags
                in Right (realFlags, nonopts)



printHelpText :: IO()
printHelpText = do
    progName <- getProgName
    putStrLn $ helpText progName
    exitSuccess


loadColour :: String -> IO(Map String String)
loadColour str = 
    case decode $ fromChunks [pack str] of
        Just map -> return map
        Nothing -> loadMapFromFile str
    where   
        loadMapFromFile :: FilePath -> IO(Map String String)
        loadMapFromFile path = do
            putStrLn "colour map not valid json, loading as file..."
            bs <- BSL.readFile path
            
            let fileColourMap = decode bs
            
            case fileColourMap of 
                Just m  -> return m
                Nothing -> do
                    putStrLn $ 
                        "failed to parse json in " ++ path 
                        ++ ", falling back to default" 
                    return defaultColourMap

getColourmapFromOpts :: [FLAG] -> IO(Map String String)
getColourmapFromOpts flags =
    if not $ null colourflags
        then mapOf (head colourflags)
        else return defaultColourMap

    where 
        colourflags     = filter isColourMap flags
        isColourMap arg = case arg of
            COLOUR_MAPPING _ -> True
            _                -> False
        mapOf (COLOUR_MAPPING map) = map


main :: IO()
main = do
    argv <- getArgs
    either' (getOpt argv) 
        exitWithError 
        (\opts -> do
            let (flags, filePaths) = opts

            globs <- mapM glob filePaths
            let globbedPaths = foldl' (++) [] globs

            putStrLn $ "Paths: "            ++ show filePaths
            putStrLn $ "Globbed Files: "    ++ show globbedPaths

            -- print helptext when no args or --help is one of the args
            when (HELP `elem` flags || null globbedPaths)
                printHelpText

            colourMap <- getColourmapFromOpts flags
            canonicalPaths <- mapM canonical filePaths

            print colourMap
            print canonicalPaths

            matchColoursOnFiles colourMap canonicalPaths
        )
