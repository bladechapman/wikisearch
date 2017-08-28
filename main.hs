import System.Directory
import AbsolutePath
import File
import Control.Monad

import System.Environment
import System.IO

main = (getPath "/Users/bladechapman/Developer/Haskell/wikisearch/test") >>= rGetFiles

rGetFiles :: AbsolutePath -> IO [File]
rGetFiles path = do
  pathContents <- contentsForDirectory path
  pathFiles <- filterM isFileForAbsolutePath pathContents
  pathDirectories <- filterM isDirectoryForAbsolutePath pathContents
  deep <- (flattenM . (mapM rGetFiles)) pathDirectories
  shallow <- mapM getFileForPath pathFiles
  return (concat [deep, shallow])

flattenM :: (Foldable t, Monad m) => m (t [a]) -> m ([a])
flattenM mta = do
  contents <- mta
  foldM (\a b -> return (concat [b, a])) [] contents
