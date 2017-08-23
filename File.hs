module File
(
  getFileForPath,
  File
) where

import AbsolutePath


data File = File {
  absolutePath :: IO AbsolutePath,
  contents :: IO (Maybe String)
}

getFileForPath :: AbsolutePath -> IO File
getFileForPath path = do
  fileContent <- readFileForPath path
  return (File (return path) (return fileContent))
