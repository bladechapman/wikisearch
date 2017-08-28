module File
(
  File,
  FileType (...)
  getFileForPath,
) where

import AbsolutePath


{- PUBLIC -}

data File = File {
  absolutePath :: AbsolutePath,
  contents :: Maybe String,
  fileType :: FileType
}

instance Show File where
  show (File aPath _ _) = show aPath

data FileType = Txt | Rtf | Unknown (Maybe String)

getFileForPath :: AbsolutePath -> IO File
getFileForPath path = do
  fileContent <- readFileForPath path
  return (File path fileContent (Unknown (Just "this")))
