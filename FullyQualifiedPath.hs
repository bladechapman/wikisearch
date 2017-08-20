module FullyQualifiedPath
( getPath,
  directoriesForPath,
  filesForPath,
  contentsForPath,
  readFileForPath
) where

import System.Directory
import Control.Monad


{- PUBLIC -}

-- | Verifies the given path exists in the file system
getPath :: FilePath -> IO FullyQualifiedPath
getPath path = do
  exists <- isFileOrDirectoryFromRoot path
  if exists
    then return (FullyQualifiedPath (Just path))
    else return (FullyQualifiedPath Nothing)

-- | List the directories of a fully qualified path pointing to a directory
directoriesForPath :: FullyQualifiedPath -> IO [FullyQualifiedPath]
directoriesForPath (FullyQualifiedPath Nothing) = return []
directoriesForPath fqp = do
  contentPaths <- contentsForPath fqp
  filterM ((fmap not) . isFileForFullyQualifiedPath) contentPaths

-- | List the files of a fully qualified path pointing to a directory
filesForPath :: FullyQualifiedPath -> IO [FullyQualifiedPath]
filesForPath (FullyQualifiedPath Nothing) = return []
filesForPath fqp = do
  contentPaths <- contentsForPath fqp
  filterM isFileForFullyQualifiedPath contentPaths

-- | List the full contents of a fully qualified path pointing to a directory
contentsForPath :: FullyQualifiedPath -> IO [FullyQualifiedPath]
contentsForPath (FullyQualifiedPath Nothing) = return []
contentsForPath (FullyQualifiedPath (Just path)) = do
  isDirectory <- isDirectoryForFullyQualifiedPath (FullyQualifiedPath (Just path))
  if isDirectory
    then do
      contents <- listDirectory path
      return (map (\contentPath -> FullyQualifiedPath (Just (path ++ "/" ++ contentPath))) contents)
    else
      return []

-- | Gives the contents of a file given a fully qualified path
readFileForPath :: FullyQualifiedPath -> IO String
readFileForPath (FullyQualifiedPath Nothing) = return ""
readFileForPath (FullyQualifiedPath (Just path)) = do
  isDirectory <- doesDirectoryExist path
  if isDirectory
    then return ""
    else readFile path


{- PRIVATE -}

-- | Hidden to assert validity of a fully qualified path
-- type FullyQualifiedPath = Maybe FilePath
data FullyQualifiedPath = FullyQualifiedPath (Maybe FilePath)

-- | Determines if a given file path exists and leads from root
isFileOrDirectoryFromRoot :: FilePath -> IO Bool
isFileOrDirectoryFromRoot path = do
  isFile <- doesFileExist path
  isDirectory <- doesDirectoryExist path
  isFromRoot <- return ((head path) == '/')
  return ((isFile || isDirectory) && isFromRoot)

-- | Determines if a given fully qualified path points to a file
isFileForFullyQualifiedPath :: FullyQualifiedPath -> IO Bool
isFileForFullyQualifiedPath (FullyQualifiedPath Nothing) = return False
isFileForFullyQualifiedPath (FullyQualifiedPath (Just path)) = doesFileExist path

-- | Determines if a given fully qualified path points to a file
isDirectoryForFullyQualifiedPath :: FullyQualifiedPath -> IO Bool
isDirectoryForFullyQualifiedPath (FullyQualifiedPath Nothing) = return False
isDirectoryForFullyQualifiedPath (FullyQualifiedPath (Just path)) = doesDirectoryExist path
