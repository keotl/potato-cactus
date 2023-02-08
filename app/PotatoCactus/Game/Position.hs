module PotatoCactus.Game.Position where

data Position = Position
  { x :: Int,
    y :: Int,
    z :: Int
  } deriving (Show)

-- Bottom-left coordinate of chunk
chunkX :: Position -> Int
chunkX pos =
  (x pos `div` 8) - 6

chunkY :: Position -> Int
chunkY pos =
  (y pos `div` 8) - 6

class GetPosition t where
  getPosition :: t -> Position
