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
  getPath,
  directoriesForDirectory,
  filesForDirectory,
  contentsForDirectory,
  readFileForPath,
  AbsolutePath
) where

import System.Directory
import Control.Monad


{- PUBLIC -}

-- | Verifies the given path exists in the file system
getPath :: FilePath -> IO AbsolutePath
getPath path = do
  exists <- isFileOrDirectoryFromRoot path
  if exists
    then return (AbsolutePath (Just path))
    else return (AbsolutePath Nothing)

-- | List the directories of a fully qualified path pointing to a directory
directoriesForDirectory :: AbsolutePath -> IO [AbsolutePath]
directoriesForDirectory (AbsolutePath Nothing) = return []
directoriesForDirectory fqp = do
  contentPaths <- contentsForDirectory fqp
  filterM ((fmap not) . isFileForAbsolutePath) contentPaths

-- | List the files of a fully qualified path pointing to a directory
filesForDirectory :: AbsolutePath -> IO [AbsolutePath]
filesForDirectory (AbsolutePath Nothing) = return []
filesForDirectory fqp = do
  contentPaths <- contentsForDirectory fqp
  filterM isFileForAbsolutePath contentPaths

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
    else (readFile path) >>= (\x -> return (Just x))


{- PRIVATE -}

-- | Constructor hidden to assert validity of a fully qualified path
data AbsolutePath = AbsolutePath (Maybe FilePath) deriving (Show)

-- | Determines if a given file path exists and leads from root
isFileOrDirectoryFromRoot :: FilePath -> IO Bool
isFileOrDirectoryFromRoot path = do
  isFile <- doesFileExist path
  isDirectory <- doesDirectoryExist path
  isFromRoot <- return ((head path) == '/')
  return ((isFile || isDirectory) && isFromRoot)

-- | Determines if a given fully qualified path points to a file
isFileForAbsolutePath :: AbsolutePath -> IO Bool
isFileForAbsolutePath (AbsolutePath Nothing) = return False
isFileForAbsolutePath (AbsolutePath (Just path)) = doesFileExist path

-- | Determines if a given fully qualified path points to a file
isDirectoryForAbsolutePath :: AbsolutePath -> IO Bool
isDirectoryForAbsolutePath (AbsolutePath Nothing) = return False
isDirectoryForAbsolutePath (AbsolutePath (Just path)) = doesDirectoryExist path
