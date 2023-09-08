{-# LANGUAGE MultiWayIf #-}

module PotatoCactus.Game.Entity.Interaction.ClosestInteractableTileCalc (selectClosestInteractableTile) where

import PotatoCactus.Game.Position (Position (Position, x, y, z))
import PotatoCactus.Utils.Flow ((|>))

selectClosestInteractableTile :: (Int, Int) -> Position -> Position -> Position
selectClosestInteractableTile (sizeX, sizeY) entityPos actorPos =
  let (minX, minY) = (x entityPos - 1, y entityPos - 1)
   in let (maxX, maxY) = (x entityPos + sizeX, y entityPos + sizeY)
       in Position
            (pickWithinBounds_ (minX, maxX) (x actorPos))
            (pickWithinBounds_ (minY, maxY) (y actorPos))
            (z entityPos)
            |> disallowCorner_ (minX, minY) (maxX, maxY)
            |> disallowInside_ (minX, minY) (maxX, maxY)

pickWithinBounds_ :: (Int, Int) -> Int -> Int
pickWithinBounds_ (minVal, maxVal) value
  | value < minVal = minVal
  | value > maxVal = maxVal
  | otherwise = value

disallowCorner_ :: (Int, Int) -> (Int, Int) -> Position -> Position
disallowCorner_ (minX, minY) (maxX, maxY) pos
  | x pos == minX && y pos == minY = pos {x = x pos + 1}
  | x pos == minX && y pos == maxY = pos {x = x pos + 1}
  | x pos == maxX && y pos == maxY = pos {x = x pos - 1}
  | x pos == maxX && y pos == minY = pos {x = x pos - 1}
  | otherwise = pos

disallowInside_ :: (Int, Int) -> (Int, Int) -> Position -> Position
disallowInside_ (minX, minY) (maxX, maxY) pos
  | x pos <= minX || x pos >= maxX = pos
  | y pos <= minY || y pos >= maxY = pos
  | otherwise =
    let (distanceToMinX, distanceToMaxX) = (x pos - minX, maxX - x pos)
     in let (distanceToMinY, distanceToMaxY) = (y pos - minY, maxY - y pos)
         in let bestDistance = minimum [distanceToMinX, distanceToMaxX, distanceToMinY, distanceToMaxY]
             in if
                    | bestDistance == distanceToMinX -> pos {x = minX}
                    | bestDistance == distanceToMinY -> pos {y = minY}
                    | bestDistance == distanceToMaxX -> pos {x = maxX}
                    | bestDistance == distanceToMaxY -> pos {y = maxY}
                    | otherwise -> pos -- Should not happen
