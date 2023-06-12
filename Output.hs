module Output
(
  saveToTextFile
) where

import DataStructure
import System.IO

saveToTextFile strContents strFilename = do
    hndFile <- openFile strFilename WriteMode
    hPutStrLn hndFile strContents
    hClose hndFile