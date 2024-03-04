module Lib
    ( cli
    , parseArgs
    ) where

import System.Environment
import System.Exit
import System.IO

type ErrorMessage = String
type FileName = String

exitWithErrorMessage :: ErrorMessage -> ExitCode -> IO ()
exitWithErrorMessage m e = hPutStrLn stderr m >> exitWith e

parseArgs :: [String] -> Either FileName ErrorMessage
parseArgs xs = if length xs == 1
               then Left (head xs)
               else Right "expected one argument"

printFile :: FileName -> IO ()
printFile x = do
  contents <- readFile x
  putStrLn contents

cli :: IO ()
cli = do
  args <- getArgs
  case parseArgs args of
    Left x -> printFile x
    Right x -> exitWithErrorMessage ("error: " ++ x) (ExitFailure 1)
