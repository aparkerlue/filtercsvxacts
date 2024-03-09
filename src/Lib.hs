module Lib
    ( cli
    , parseArgs
    ) where

import Data.List (intercalate)
import qualified Data.Map as Map
import System.Environment
import System.Exit
import System.IO

type ErrorMessage = String
type FileName = String

exitWithErrorMessage :: ErrorMessage -> ExitCode -> IO ()
exitWithErrorMessage m e = hPutStrLn stderr m >> exitWith e

parseArgs :: [String] -> Either [FileName] ErrorMessage
parseArgs xs@(_:_) = Left xs
parseArgs [] = Right "expected at least one argument"

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

data ParsedField = ParsedField
  { fieldContent :: String
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
    Just (ParsedField {fieldContent = x, endOfRecord = True})
      -> (fs ++ [x], rest)
    Just (ParsedField {fieldContent = x, endOfRecord = False})
      | rest == "" -> (fs ++ [x,""], "")
      | otherwise -> nextRecord' (fs ++ [x]) rest
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

uniqConcat :: [String] -> [String] -> [String]
uniqConcat xs ys = xs ++ [y | y <- ys, not $ elem y xs]

type StrTable = [Map.Map String String]
type Header = [String]

data StrDoc = StrDoc
  { headerOrder :: Header
  , docRecords :: StrTable
  } deriving (Show)

emtpyStrDoc :: StrDoc
emtpyStrDoc = StrDoc [] []

stHeader :: StrDoc -> [String]
stHeader doc = first ++ [x | x <- found, not $ elem x first]
  where first = headerOrder doc
        found = foldr uniqConcat [] $ map Map.keys $ docRecords doc

stRecord :: Header -> Map.Map String String -> Record
stRecord hs m = [case v of Just x -> x
                           Nothing -> ""
                | v <- map (\h -> Map.lookup h m) hs]

stRecords :: StrDoc -> [Record]
stRecords x = map (stRecord $ stHeader x) $ docRecords x

convRecordsToStrDoc :: [Record] -> StrDoc
convRecordsToStrDoc [] = StrDoc [] []
convRecordsToStrDoc (h:rs) = StrDoc h $ map (\x -> Map.fromList $ zip h x) rs

convStrDocToString :: StrDoc -> String
convStrDocToString xs = intercalate "\n" $ map (intercalate "|") $ header:records
  where header = stHeader xs
        records = stRecords xs

concatStrDocs :: StrDoc -> StrDoc -> StrDoc
concatStrDocs x y = StrDoc headers records
  where xheaders = headerOrder x
        yheaders = headerOrder y
        headers = xheaders ++ filter (\z -> not $ elem z xheaders) yheaders
        xrecords = docRecords x
        yrecords = docRecords y
        records = xrecords ++ filter (\z -> not $ elem z xrecords) yrecords

printStrDoc :: StrDoc -> IO ()
printStrDoc x = do
  putStrLn $ convStrDocToString x
  putStrLn ""
  putStrLn $ "Processed " ++ (show $ length $ docRecords x) ++ " records."

cli :: IO ()
cli = do
  args <- getArgs
  case parseArgs args of
    Left xs -> do
      ys <- mapM readFile xs
      printStrDoc $ foldr concatStrDocs emtpyStrDoc
        $ map (convRecordsToStrDoc . parseCsv) ys
    Right x -> exitWithErrorMessage ("error: " ++ x) (ExitFailure 1)
