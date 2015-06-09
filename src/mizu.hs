module Main where

import Data.List (foldl', intercalate, repeat, intersect)

-- Simple stuff for IO
import System.IO (stdin)
import System.Exit (exitWith, exitSuccess, ExitCode(..))

import System.FilePath.Canonical
import System.Console.GetOpt (
    getOpt',
    OptDescr(..),
    ArgDescr(..),
    ArgOrder(Permute))

import Control.Monad (when)

data MaybeFlag = Error String | Flag FLAG

-- options
data FLAG = HELP
    deriving (Show, Eq)

(+\) a b = a ++ "\n" ++ b
(+\\) a b = a ++ "\n    " ++ b

-- help text
helpText :: String -> String
helpText progName =
    "usage: " ++ progName ++ " [options] [files...]" +\
    "[options] one of" +\\
        intercalate "\n    " optionStrings +\
    "And files is one of the "

optionStrings :: [String]
optionStrings =
    let pairs =  map (\ (Option shorts longs _ desc) ->
                    let fnames = map (\c -> ["-",c]) shorts
                              ++ map (\l -> "--" ++ l) longs
                    in (intercalate ", " fnames, desc)) options
        maxflagstrlen = maximum $ map (\ (flagstr, _) -> length flagstr) pairs
    in map (\ (flagstr, desc) ->
        flagstr
        ++ replicate (8 + maxflagstrlen - length flagstr) " "
        ++ desc) pairs


-- opt definitions
helpOpt =
    Option ["h"] ["help"]
        (NoArg (Flag HELP))
        "display helptext and exit"

outdirOpt =
    Option ["o"] ["output-directory"]
        (ReqArg mapping "PATH")
        "Output directory (place the templates will be placed)."+\
        "If left unspecified, templates will be left next to source files"
    where mapping = id

destdirOpt =
    Option ["d"] ["template-destination"]
        (ReqArg mapping "PATH")
        "generation method (kmeans/..)"
    where mapping = id

coloursOpt =
    Option ["c"] ["colour", "color"]
        (ReqArg mapping "FORMAT")
        "output format (xresources/xgcm)"
    where mapping arg = 
            case arg of
                "xresources" -> Flag FORMAT_XRESOURCES
                "xgcm"       -> Flag FORMAT_XGCM
                u            -> Error ("Unknown format " ++ show u)

options :: [OptDescr MaybeFlag]
options =
    [ helpOpt
    , backgroundOpt
    , methodOpt
    , formatOpt ]


getOpt :: [String] -> Either String ([FLAG], [String])
getOpt argv =
    let (flags, nonopts, unknownopts, errs) = getOpt' Permute options argv
        flagErrors = filter
            (\ f -> case f of
                Error _ -> True
                _       -> False) flags

    in if not null flagErrors -- if flagErrors is empty
        then let flagErrorMessages = map (\ (Error s) -> s) flagErrors
            in Left ("Error in parsing flags:\n\t"
                        ++ intercalate "\n\t" flagErrorMessages)
        else if not null errs
            then Left ("Error parsing argv\n" ++ foldl' (++) "" errs)
            else let realFlags = map (\ (Flag f) -> f) flags
                in Right (realFlags, nonopts)

-- split path on image decoding error
either' switch fail succeed = either fail succeed switch

printHelpText :: IO()
printHelpText = do
    progName <- getProgName
    putStrLn $ helpText progName
    exitSuccess

parseFiles :: []

main :: IO()
main = do
    argv <- getArgs
    either' (getOpt argv) 
        exitWithError 
        (\opts -> do
            let (flags, filePaths) = opts

            when (HELP `elem` flags || null filePaths)
                printHelpText

            let canonicalPaths = map canonical filePaths
            scanColours canonicalPaths
            )
