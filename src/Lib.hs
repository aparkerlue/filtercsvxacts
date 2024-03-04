module Lib
    ( cli
    , parseArgs
    ) where

import Data.List (elemIndex, intercalate)
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

nonEscDQuoteIndex :: String -> Maybe Int
nonEscDQuoteIndex "" = Nothing
nonEscDQuoteIndex ('"':'"':xs) = nonEscDQuoteIndex xs
nonEscDQuoteIndex ('"':_) = Just 0
nonEscDQuoteIndex (_:xs) = case nonEscDQuoteIndex xs of Just n -> Just (n + 1)
                                                        Nothing -> Nothing

unescDQuotes :: String -> String
unescDQuotes ('"':'"':xs) = '"':(unescDQuotes xs)
unescDQuotes (x:xs) = x:(unescDQuotes xs)
unescDQuotes xs = xs

splitNextField :: String -> (String, String)
splitNextField (',':xs) = splitNextField xs
splitNextField xss@('"':xs) = case nonEscDQuoteIndex xs of
                                Just i -> (unescDQuotes (take i xs)
                                          , drop (i + 1) xs)
                                Nothing -> (xss, "")
splitNextField xs = case elemIndex ',' xs of
                      Just i -> (take i xs, drop (i + 1) xs)
                      Nothing -> (xs, "")

parseFields :: [String] -> String -> [String]
parseFields xs "" = xs
parseFields xs s = parseFields (xs ++ [field]) rest
  where (field, rest) = splitNextField s

parseRecord :: String -> [String]
parseRecord r = parseFields [] r

printCSVFile :: FileName -> IO ()
printCSVFile x = do
  contents <- readFile x
  let records = lines contents
  _ <- mapM putStrLn (map (intercalate "|" . parseRecord) records)
  return ()

cli :: IO ()
cli = do
  args <- getArgs
  case parseArgs args of
    Left x -> printCSVFile x
    Right x -> exitWithErrorMessage ("error: " ++ x) (ExitFailure 1)
