module DataStructure
( Dimensions
, Position
, Circle
, Circles
, Colour
, CircleSet(..)
, Pixel(..)
, Image(..)
) where

type Dimensions = (Int, Int)
type Position = (Int, Int)
type Colour = (Int, Int, Int)
type Circle = (Position, Double, Colour)
type Circles = [Circle]

data CircleSet = CircleSet { dimensions :: Dimensions
                           , circles :: Circles
                           } deriving (Show, Read)

data Pixel = Pixel { position :: Position
                   , colour :: Colour
                   } deriving (Show, Read)
                
data Image = Image { size :: Dimensions
                   , pixels :: [Pixel]
                   } deriving (Show, Read)