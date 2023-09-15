module PotatoCactus.Game.Movement.Direction where

import PotatoCactus.Game.Movement.PositionXY (PositionXY (x, y))

data Direction = None | NorthWest | North | NorthEast | West | East | SouthWest | South | SouthEast deriving (Show, Eq)

directionId :: Direction -> Int
directionId None = -1
directionId NorthWest = 0
directionId North = 1
directionId NorthEast = 2
directionId West = 3
directionId East = 4
directionId SouthWest = 5
directionId South = 6
directionId SouthEast = 7

directionBetween :: PositionXY -> PositionXY -> Direction
directionBetween current next =
  case (deltaX, deltaY) of
    (1, 1) -> NorthEast
    (0, 1) -> North
    (-1, 1) -> NorthWest
    (1, -1) -> SouthEast
    (0, -1) -> South
    (-1, -1) -> SouthWest
    (1, 0) -> East
    (-1, 0) -> West
    _ -> None
  where
    deltaX = capDelta_ (x next - x current)
    deltaY = capDelta_ (y next - y current)

capDelta_ :: Int -> Int
capDelta_ x
  | x > 1 = 1
  | x < -1 = -1
  | otherwise = x
