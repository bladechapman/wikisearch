import System.Directory
import AbsolutePath
import File
import Control.Monad

import System.Environment
import System.IO

main = getCurrentDirectory

rGetFiles :: AbsolutePath -> IO [File]
rGetFiles path = do
  pathContents <- contentsForDirectory path
  pathFiles <- filterM isFileForAbsolutePath pathContents
  pathDirectories <- filterM isDirectoryForAbsolutePath pathContents
  mapM getFileForPath pathFiles
