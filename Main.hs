module Main
( main
) where
import DataStructure
import Input
import Output
import Processing
import System.Environment
import Data.List

main = do
    lstArgs <- getArgs
    let strInputFileName = head lstArgs
    let strOutputFileName = lstArgs !! 1
    recCircleSet <- readCircleSetFromFile strInputFileName
    let imageSize = dimensions recCircleSet
    let lstCircles = circles recCircleSet
    putStrLn "Generating Canvas..."
    let imgCanvas = createImage imageSize (255,255,255)
    putStrLn "Processing Circles..."
    let lstBlendedImages = reverse $ blendCirclesWithImage imgCanvas lstCircles
    putStrLn "Converting To PPM..."
    let strPPM = convertToPPM $ head lstBlendedImages
    saveToTextFile  strPPM strOutputFileName
    putStrLn "PPM File Generated"

    