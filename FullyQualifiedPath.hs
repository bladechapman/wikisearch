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
    then return (Just path)
    else return Nothing

-- | List the directories of a fully qualified path pointing to a directory
directoriesForPath :: FullyQualifiedPath -> IO [FullyQualifiedPath]
directoriesForPath Nothing = return []
directoriesForPath (Just path) = do
  contentPaths <- contentsForPath (Just path)
  filterM ((fmap not) . isFileForFullyQualifiedPath) contentPaths

-- | List the files of a fully qualified path pointing to a directory
filesForPath :: FullyQualifiedPath -> IO [FullyQualifiedPath]
filesForPath Nothing = return []
filesForPath (Just path) = do
  contentPaths <- contentsForPath (Just path)
  filterM isFileForFullyQualifiedPath contentPaths

-- | List the full contents of a fully qualified path pointing to a directory
contentsForPath :: FullyQualifiedPath -> IO [FullyQualifiedPath]
contentsForPath Nothing = return []
contentsForPath (Just path) = do
  isDirectory <- isDirectoryForFullyQualifiedPath (Just path)
  if isDirectory
    then do
      contents <- listDirectory path
      return (map (\contentPath -> Just (path ++ "/" ++ contentPath)) contents)
    else
      return []

-- | Gives the contents of a file given a fully qualified path
readFileForPath :: FullyQualifiedPath -> IO String
readFileForPath Nothing = return ""
readFileForPath (Just path) = do
  isDirectory <- doesDirectoryExist path
  if isDirectory
    then return ""
    else readFile path


{- PRIVATE -}

-- | Hidden to assert validity of a fully qualified path
type FullyQualifiedPath = Maybe FilePath

-- | Determines if a given file path exists and leads from root
isFileOrDirectoryFromRoot :: FilePath -> IO Bool
isFileOrDirectoryFromRoot path = do
  isFile <- doesFileExist path
  isDirectory <- doesDirectoryExist path
  isFromRoot <- return ((head path) == '/')
  return ((isFile || isDirectory) && isFromRoot)

-- | Determines if a given fully qualified path points to a file
isFileForFullyQualifiedPath :: FullyQualifiedPath -> IO Bool
isFileForFullyQualifiedPath Nothing = return False
isFileForFullyQualifiedPath (Just path) = doesFileExist path

-- | Determines if a given fully qualified path points to a file
isDirectoryForFullyQualifiedPath :: FullyQualifiedPath -> IO Bool
isDirectoryForFullyQualifiedPath Nothing = return False
isDirectoryForFullyQualifiedPath (Just path) = doesDirectoryExist path
