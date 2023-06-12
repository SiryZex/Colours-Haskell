module Input
(
  readCircleSetFromFile
) where
import System.IO
import DataStructure

readCircleSetFromFile strFilename = do
    hndFile <- openFile strFilename ReadMode
    strContents <- hGetContents hndFile
    let recCircleSet = read strContents :: CircleSet
    putStr "Reading from file:"
    putStrLn $ show recCircleSet
    hClose hndFile
    return recCircleSet