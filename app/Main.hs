{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Parsers
import Text.Parsec
import Structure
import Assembly
import System.Process
import System.Environment
import Data.FileEmbed (embedStringFile)
import Data.List
import Control.Monad
import System.Console.GetOpt
import System.IO
import System.Exit
import Data.List.Split
import Data.Text (replace, unpack, pack)

usage :: String -> String
usage name = unpack $ replace "kpyc" (pack name) $ pack $(embedStringFile "usage.txt")

data Options = Options { optExecute :: Bool, optDebug :: Bool, optOutput :: String }

defaultOptions :: Options
defaultOptions = Options { optExecute = False, optDebug = False, optOutput = "" }

options :: [OptDescr (Options -> IO Options)]
options =
    [ Option "o" ["output"] (ReqArg (\arg opt -> return opt { optOutput = arg }) "FILE") "Specify the name of the final executable"
    , Option "e" ["execute"] (NoArg (\opt -> return opt { optExecute = True })) "Execute after compilation"
    , Option "d" ["debug"] (NoArg (\opt -> return opt { optDebug = True })) "Add debug symbols"
    , Option "h" ["help"] (NoArg (\_ -> do name <- getProgName; hPutStrLn stderr $ usage name; exitSuccess)) "Show help"
    ]

main :: IO ()
main = do
    args <- getArgs
    let (actions, nonOptions, _) = getOpt Permute options args
    Options { optExecute = execute, optOutput = output, optDebug = debug } <- foldl (>>=) (return defaultOptions) actions
    input <- case nonOptions of
        [filePath] -> return filePath
        _ -> do
            hPutStrLn stderr $ "Error: A single file argument is required. " ++ show nonOptions
            exitFailure
    let name = if output == "" then intercalate "." . init $ splitOn "." input else output
    pythonCode <- readFile input
    compile debug pythonCode name
    when execute $ callProcess ("./" ++ name) []

compile :: Bool -> String -> String -> IO ()
compile debug input name = do
    let result = runProgramParser input
    case result of
        Left parseError -> do
            putStr "Error "
            print parseError
            exitFailure
        Right program -> do
            let assembly = execBuilder program
            writeFile (name ++ ".asm") assembly
            let nasmArgs = if debug then ["-f", "elf64", "-F", "dwarf", name ++ ".asm"] else ["-f", "elf64", name ++ ".asm"]
            callProcess "nasm" nasmArgs
            callProcess "ld" [name ++ ".o", "-o", name]

runProgramParser :: String -> Either ParseError Program
runProgramParser = applyParserWithState programP defaultProgram
