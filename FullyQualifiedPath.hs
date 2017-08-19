module FullyQualifiedPath
( getPath,
  directoriesForPath,
  filesForPath
) where

import System.Directory
import Control.Monad


{- PUBLIC -}

-- | Verifies the given path exists in the file system
getPath :: FilePath -> IO FullyQualifiedPath
getPath path = do
  exists <- isFileOrDirectoryFromRoot path
  if exists
    then return ((Just path) :: FullyQualifiedPath)
    else return Nothing

-- | List the directories present within a fully qualified path
directoriesForPath :: FullyQualifiedPath -> IO [FullyQualifiedPath]
directoriesForPath Nothing = return []
directoriesForPath (Just path) = do
  contents <- listDirectory path
  contentPaths <- return (map (\contentPath -> path ++ "/" ++ contentPath) contents)
  directoryPaths <- ((filterM ((fmap not) . doesFileExist)) contentPaths)
  return (map Just (directoryPaths))

-- | List the files present within a fully qualified path
filesForPath :: FullyQualifiedPath -> IO [FullyQualifiedPath]
filesForPath Nothing = return []
filesForPath (Just path) = do
  contents <- listDirectory path
  contentPaths <- return (map (\contentPath -> path ++ "/" ++ contentPath) contents)
  filePaths <- filterM doesFileExist contentPaths
  return (map Just (filePaths))



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
