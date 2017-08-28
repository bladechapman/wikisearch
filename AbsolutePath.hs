{-
Defines the behavior of a "fully qualified path".

A fully qualified path is a path to a valid entry in the file system
(at the time of lookup), and contains every path component between the
target and root.

For example, if my current directory structure is
```
test
|- A.txt
|- B.txt
```
the fully qualified path of A.txt would be something like
```
/Users/myName/some/files/leading/to/test/A.txt
```
-}

module AbsolutePath
(
  AbsolutePath,
  getPath,
  contentsForDirectory,
  readFileForPath,
  isFileForAbsolutePath,
  isDirectoryForAbsolutePath
) where

import Control.Exception
import Control.Monad
import Data.Typeable
import System.Directory
import System.Environment
import System.IO


{- PUBLIC -}

-- | Verifies the given path exists in the file system
getPath :: FilePath -> IO AbsolutePath
getPath path = do
  exists <- isFileOrDirectoryFromRoot path
  if exists
    then return (AbsolutePath (Just path))
    else return (AbsolutePath Nothing)

-- | List the full contents of a fully qualified path pointing to a directory
contentsForDirectory :: AbsolutePath -> IO [AbsolutePath]
contentsForDirectory (AbsolutePath Nothing) = return []
contentsForDirectory (AbsolutePath (Just path)) = do
  isDirectory <- isDirectoryForAbsolutePath (AbsolutePath (Just path))
  if isDirectory
    then do
      contents <- listDirectory path
      return (map (\contentPath -> AbsolutePath (Just (path ++ "/" ++ contentPath))) contents)
    else
      return []

-- | Gives the contents of a file given a fully qualified path
readFileForPath :: AbsolutePath -> IO (Maybe String)
readFileForPath (AbsolutePath Nothing) = return Nothing
readFileForPath (AbsolutePath (Just path)) = do
  isDirectory <- doesDirectoryExist path
  if isDirectory
    then return Nothing
    else (safeReadFile path) >>= (\x -> return (Just x))

-- | Determines if a given fully qualified path points to a file
isFileForAbsolutePath :: AbsolutePath -> IO Bool
isFileForAbsolutePath (AbsolutePath Nothing) = return False
isFileForAbsolutePath (AbsolutePath (Just path)) = doesFileExist path

-- | Determines if a given fully qualified path points to a file
isDirectoryForAbsolutePath :: AbsolutePath -> IO Bool
isDirectoryForAbsolutePath (AbsolutePath Nothing) = return False
isDirectoryForAbsolutePath (AbsolutePath (Just path)) = doesDirectoryExist path


{- PRIVATE -}

-- | Constructor hidden to assert validity of a fully qualified path
data AbsolutePath = AbsolutePath (Maybe FilePath)
instance Show AbsolutePath where
  show (AbsolutePath (Just str)) = str
  show (AbsolutePath Nothing) = ""

-- | Determines if a given file path exists and leads from root
isFileOrDirectoryFromRoot :: FilePath -> IO Bool
isFileOrDirectoryFromRoot path = do
  isFile <- doesFileExist path
  isDirectory <- doesDirectoryExist path
  isFromRoot <- return ((head path) == '/')
  return ((isFile || isDirectory) && isFromRoot)

-- | Reads a file in latin1 encoding.
-- this is to get aroudn the
-- hGetContents: invalid argument (invalid byte sequence) exception
safeReadFile :: FilePath -> IO String
safeReadFile path = do
  h <- openFile path ReadMode
  hSetEncoding h latin1
  hGetContents h
