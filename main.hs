import System.Directory
import AbsolutePath
import File

main = getCurrentDirectory

-- getCurrentDirectory >>= getPath >>= filesForPath



test = getCurrentDirectory >>= getPath >>= getFileForPath
