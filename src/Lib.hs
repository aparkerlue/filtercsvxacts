module Lib
    ( cli
    , parseArgs
    ) where

import Data.List (intercalate)
import System.Environment
import System.Exit
import System.IO

type ErrorMessage = String
type FileName = String

exitWithErrorMessage :: ErrorMessage -> ExitCode -> IO ()
exitWithErrorMessage m e = hPutStrLn stderr m >> exitWith e

parseArgs :: [String] -> Either FileName ErrorMessage
parseArgs (x:[]) = Left x
parseArgs _ = Right "expected one argument"

-- |Return the index of the closing quote of a double-quoted string.
-- This function assumes that the opening quote is not present in the
-- argument.
quoteEndIndex :: String -> Int
quoteEndIndex "" = 0
quoteEndIndex ('"':'"':xs) = 2 + quoteEndIndex xs
quoteEndIndex ('"':_) = 1
quoteEndIndex (_:xs) = 1 + quoteEndIndex xs

-- |Return the index of the end of the first CSV field in the string.
fieldEndIndex :: String -> Int
fieldEndIndex "" = 0
fieldEndIndex ('\n':_) = 0
fieldEndIndex ('\r':'\n':_) = 0
fieldEndIndex ('"':xs) = 1 + i + (fieldEndIndex $ drop i xs)
  where i = quoteEndIndex xs
fieldEndIndex (',':_) = 0
fieldEndIndex (_:xs) = 1 + fieldEndIndex xs

unquoteQuotedFieldRem :: String -> String
unquoteQuotedFieldRem "" = ""
unquoteQuotedFieldRem ('"':'"':xs) = '"':unquoteQuotedFieldRem xs
unquoteQuotedFieldRem ('"':xs) = unquoteField xs
unquoteQuotedFieldRem (x:xs) = x:unquoteQuotedFieldRem xs

unquoteField :: String -> String
unquoteField "" = ""
unquoteField ('"':xs) = unquoteQuotedFieldRem xs
unquoteField (x:xs) = x:unquoteField xs

data ParsedField = ParsedField { fieldContent :: String
                               , endOfRecord :: Bool
                               } deriving (Show)

nextField :: String -> (Maybe ParsedField, String)
nextField "" = (Nothing, "")
nextField xs
  | rest /= xs = (Just $ ParsedField field eor, rest)
  | otherwise = (Nothing, xs)
  where i = fieldEndIndex xs
        field = unquoteField $ take i xs
        (fieldEnd, rest) = case drop i xs of
                             "" -> ("", "")
                             '\n':ys -> ("\n", ys)
                             '\r':'\n':ys -> ("\r\n", ys)
                             ',':ys -> (",", ys)
                             ys -> ("", ys)
        eor = fieldEnd /= ","

type Record = [String]

nextRecord' :: Record -> String -> (Record, String)
nextRecord' fs xs =
  case mpf of
    Just (ParsedField {fieldContent = f, endOfRecord = True})
      -> (fs ++ [f], rest)
    Just (ParsedField {fieldContent = f, endOfRecord = False})
      | rest == "" -> (fs ++ [f,""], "")
      | otherwise -> nextRecord' (fs ++ [f]) rest
    Nothing -> (fs, rest)
  where (mpf, rest) = nextField xs

nextRecord :: String -> (Record, String)
nextRecord xs = nextRecord' [] xs

parseCsv' :: [Record] -> String -> [Record]
parseCsv' rs xs =
  case r of
    [] -> rs
    _ -> parseCsv' (rs ++ [r]) rest
  where (r, rest) = nextRecord xs

parseCsv :: String -> [Record]
parseCsv s = parseCsv' [] s

printCsvFile :: FileName -> IO ()
printCsvFile x = do
  contents <- readFile x
  let records = parseCsv contents
  _ <- mapM (putStrLn . intercalate "|") $ records
  putStrLn ""
  putStrLn $ "Processed "
    ++ (show $ length records)
    ++ " records, possibly including a header."
  return ()

cli :: IO ()
cli = do
  args <- getArgs
  case parseArgs args of
    Left x -> printCsvFile x
    Right x -> exitWithErrorMessage ("error: " ++ x) (ExitFailure 1)
