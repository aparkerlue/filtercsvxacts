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
nonEscDQuoteIndex ('"':'"':xs) = case nonEscDQuoteIndex xs of
                                   Just n -> Just (n + 2)
                                   Nothing -> Nothing
nonEscDQuoteIndex ('"':_) = Just 0
nonEscDQuoteIndex (_:xs) = case nonEscDQuoteIndex xs of Just n -> Just (n + 1)
                                                        Nothing -> Nothing

unescDQuotes :: String -> String
unescDQuotes ('"':'"':xs) = '"':(unescDQuotes xs)
unescDQuotes (x:xs) = x:(unescDQuotes xs)
unescDQuotes xs = xs

fieldEndIndex :: String -> Int
fieldEndIndex xs = case (commaIndex, crlfIndex) of
                     (Just i, Just j) -> min i j
                     (Just i, Nothing) -> i
                     (Nothing, Just j) -> j
                     (Nothing, Nothing) -> length xs
  where commaIndex = elemIndex ',' xs
        crlfIndex = elemIndex '\n' xs

getRestOfQuotedField :: String -> (String, Maybe String)
getRestOfQuotedField xs = case nonEscDQuoteIndex xs of
                            Just i -> (unescDQuotes $ take i xs,
                                        afterQuotedField $ drop (i + 1) xs)
                            Nothing -> (xs, Nothing)
  where afterQuotedField whole@('\n':_) = Just whole
        afterQuotedField (',':ys) = Just ys
        afterQuotedField "" = Nothing
        afterQuotedField ys = Just ys

nextUnquotedField :: String -> (String, Maybe String)
nextUnquotedField xs = (take i xs, rest)
  where i = fieldEndIndex xs
        rest = case drop i xs of "" -> Nothing
                                 ',':ys -> Just ys
                                 ys -> Just ys

type Record = [String]

nextRecord' :: Record -> Maybe String -> (Record, Maybe String)
nextRecord' fs (Just "") = (fs, Nothing)
nextRecord' fs (Just ('\n':xs))
  | length xs > 0 = (fs, Just xs)
  | otherwise     = (fs, Nothing)
nextRecord' fs (Just (',':xs)) = nextRecord' (fs ++ [""]) (Just xs)
nextRecord' fs (Just ('"':xs)) = nextRecord' (fs ++ [field]) xs'
  where (field, xs') = getRestOfQuotedField xs
nextRecord' fs (Just xs) = nextRecord' (fs ++ [field]) xs'
  where (field, xs') = nextUnquotedField xs
nextRecord' fs Nothing = (fs, Nothing)

nextRecord :: String -> (Record, Maybe String)
nextRecord x = nextRecord' [] (Just x)

parseCsv' :: [Record] -> Maybe String -> [Record]
parseCsv' rs (Just x) = parseCsv' (rs ++ [r]) x'
  where (r, x') = nextRecord x
parseCsv' rs Nothing = rs

parseCsv :: String -> [Record]
parseCsv s = parseCsv' [] (Just s)

printCsvFile :: FileName -> IO ()
printCsvFile x = do
  contents <- readFile x
  _ <- mapM (putStrLn . intercalate "|") $ parseCsv contents
  return ()

cli :: IO ()
cli = do
  args <- getArgs
  case parseArgs args of
    Left x -> printCsvFile x
    Right x -> exitWithErrorMessage ("error: " ++ x) (ExitFailure 1)
