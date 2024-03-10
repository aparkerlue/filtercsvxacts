-- | Directory manipulation tools.

module Directory (listDirectoryRecursive) where

import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (dropTrailingPathSeparator, joinPath)

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
