-- | Directory manipulation tools.

module Directory (hasSuffix, listDirectoryRecursive) where

import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (dropTrailingPathSeparator, joinPath)

-- |Return true if the file path ends in the suffix, false otherwise.
hasSuffix :: String -> FilePath -> Bool
hasSuffix s x = drop (max (length x - length suffix) 0) x == suffix
  where suffix = '.':s

-- |Recursively list files and directories in the file path argument.
-- This function includes the argument in the return value.
listDirectoryRecursive :: FilePath -> IO [FilePath]
listDirectoryRecursive x = do
  isDir <- doesDirectoryExist x
  if isDir
    then do
      entries <- listDirectory x
      entsrec <- mapM (listDirectoryRecursive . joinPath . (:) x . (:[])) entries
      return $ dropTrailingPathSeparator x : concat entsrec
    else do
    isFile <- doesFileExist x
    return $ if isFile
             then [x]
             else []
