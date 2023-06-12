module Processing
( calcDistance
, createPixels
, createImage
, convertToPPM
, addCircle
, blendCirclesWithImage
) where

import DataStructure

calcDistance :: Position -> Position -> Double
calcDistance p q = sqrt (s)
                 where s = fromIntegral $ a + b
                       a = (p1 - q1) ^ 2
                       b = (p2 - q2) ^ 2
                       p1 = fst p
                       q1 = fst q
                       p2 = snd p
                       q2 = snd q
                
isInRadius :: Pixel -> Circle -> Bool
isInRadius recPixel circle = calcDistance pPos cPos <= radius
                            where pPos = position recPixel
                                  (cPos, radius, _) = circle

createPixels :: Dimensions -> Colour -> [Pixel]
createPixels (rows, cols) col = let
                                    rs = [1..rows]
                                    cs = [1..cols]
                                    ps = [(r,c) | r <- rs, c <- cs]
                                in [Pixel {position = p, colour = col} | p <- ps]

createImage :: Dimensions -> Colour -> Image
createImage dim colour = Image {size = dim, pixels = createPixels dim colour}

colourToString :: Colour -> String
colourToString (red, green, blue) = strRed ++ " " ++ strGreen ++ " " ++ strBlue ++ " "
                                  where strRed = show red
                                        strGreen = show green
                                        strBlue = show blue

toPixelString :: [Pixel] -> String
toPixelString [] = ""
toPixelString (p:ps) = strPixel ++ toPixelString ps
                     where strPixel = colourToString c
                           c = colour p

convertToPPM :: Image -> String
convertToPPM imgCanvas = strHead ++ strPixels
                where strHead = "P3\n" ++ strDimensions ++ "255\n"
                      strDimensions = strCols ++ " " ++ strRows ++ "\n"
                      strCols = show $ snd $ dims
                      strRows = show $ fst $ dims
                      dims = size imgCanvas
                      strPixels = toPixelString $ pixels imgCanvas

blendColour :: Colour -> Colour -> Colour
blendColour c1 c2 = (blend c1R c2R, blend c1G c2G, blend c1B c2B)
                    where (c1R, c1G, c1B) = c1
                          (c2R, c2G, c2B) = c2
                          blend a b = let
                                         v1 = fromIntegral a
                                         v2 = fromIntegral b
                                      in round ((v1 + v2)/2)

blendPixels :: [Pixel] -> Circle -> [Pixel]
blendPixels [] _ = []
blendPixels (p:ps) circle
    | isInRadius p circle = Pixel {position = posP, colour = blendColour colP colC} : blendPixels ps circle
    | otherwise = p : blendPixels ps circle
    where 
      posP = position p
      colP = colour p
      (_, _, colC) = circle

addCircle :: Image -> Circle -> Image
addCircle imgCanvas circle = Image {size = s, pixels = blendPixels ps circle}
                           where s = size imgCanvas
                                 ps = pixels imgCanvas

blendCirclesWithImage :: Image -> Circles -> [Image]
blendCirclesWithImage _ [] = []
blendCirclesWithImage img (c:cs) = imgNew : blendCirclesWithImage imgNew cs
                                   where imgNew = addCircle img c