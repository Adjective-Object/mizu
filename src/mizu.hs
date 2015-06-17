module Main where

import System.IO (stdin)
import System.Exit (exitWith, exitSuccess, ExitCode(..))
import System.FilePath.Canonical
import System.Path.Glob(glob)
import System.Environment(getArgs)

import Control.Monad (when, sequence)
import Debug.Trace (trace)
import Data.List (foldl')

import ColourScan (matchColoursOnFiles)
import Args

-- cast a value to string
traceVal x = trace (show x) x

-- shorthand for exiting with an error msg
exitWithError err = do
    putStrLn err
    exitWith (ExitFailure 1)

-- split path on image decoding error
either' switch fail succeed = either fail succeed switch

main :: IO()
main = do
    argv <- getArgs
    either' (getOpt argv)
        exitWithError
        (\opts -> do
            let (flags, filePaths) = opts

            globs <- mapM glob filePaths
            let globbedPaths = foldl' (++) [] globs

            -- print helptext when no args or --help is one of the args
            when (HELP `elem` flags || null globbedPaths)
                printHelpText

            canonicalPaths <- mapM canonical filePaths

            colourMap   <- getColourmapFromOpts flags
            let outputDir = getOutputDirFromOpts flags
                translator  = getTranslatorFromOpts flags
            

            let mizuconf = MIZU_CONF { 
                  colourMap = colourMap
                , paths = map canonicalFilePath canonicalPaths
                , destinationFixed = True
                , outputDir = outputDir
                , translator = translator}

            matchColoursOnFiles mizuconf
        )
