module PotatoCactus.Game.Position where
import PotatoCactus.Config.Constants (positionBoundExponentXY_, positionBoundExponentZ_)

data Position = Position
  { x :: Int,
    y :: Int,
    z :: Int
  }
  deriving (Show, Eq)

-- Bottom-left coordinate of chunk
chunkX :: Position -> Int
chunkX pos =
  (x pos `div` 8) - 6

chunkY :: Position -> Int
chunkY pos =
  (y pos `div` 8) - 6

localX :: Position -> Int
localX pos =
  x pos - (chunkX pos * 8)

localY :: Position -> Int
localY pos =
  y pos - (chunkY pos * 8)

localToRefY :: Position -> Position -> Int
localToRefY ref pos =
  y pos - (chunkY ref * 8)

localToRefX :: Position -> Position -> Int
localToRefX ref pos =
  x pos - (chunkX ref * 8)

class GetPosition t where
  getPosition :: t -> Position

isWithin :: Int -> Position -> Position -> Bool
isWithin distance a b =
  z a == z b
    && ( let (deltaX, deltaY) = (x a - x b, y a - y b)
          in (abs deltaX <= distance) && (abs deltaY <= distance)
       )

-- isNextTo in 4-neighbour connectivity
isAdjacent :: Position -> Position -> Bool
isAdjacent a b =
  z a == z b
    && ( let (deltaX, deltaY) = (x a - x b, y a - y b)
          in (abs deltaX == 1 && abs deltaY == 0) || (abs deltaX == 0 && abs deltaY == 1)
       )

faraway :: Position -> Position
faraway refPos =
  refPos {x = 666666, y = 666666}

positionHash :: Position -> Int
positionHash position =
    x position
    + y position * 10 ^ keyExponentY_
    + z position * 10 ^ keyExponentZ_

keyExponentY_ = positionBoundExponentXY_

keyExponentZ_ = 2 * positionBoundExponentXY_

keyExponentObjType_ = (2 * positionBoundExponentXY_) + positionBoundExponentZ_
