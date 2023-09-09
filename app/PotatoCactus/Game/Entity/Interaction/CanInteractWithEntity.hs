module PotatoCactus.Game.Entity.Interaction.CanInteractWithEntity (canInteractWithEntity, isInsideEntity) where

import PotatoCactus.Game.Position (Position (x, y, z))

-- Assumes entityPos is bottom-left corner of entity
canInteractWithEntity :: (Int, Int) -> Position -> Position -> Bool
canInteractWithEntity (sizeX, sizeY) entityPos actorPos =
  let (minX, minY) = (x entityPos - 1, y entityPos - 1)
   in let (maxX, maxY) = (x entityPos + sizeX, y entityPos + sizeY)
       in z actorPos == z entityPos
            && ( (x actorPos == minX && y actorPos > minY && y actorPos < maxY)
                   || (x actorPos == maxX && y actorPos > minY && y actorPos < maxY)
                   || (y actorPos == minY && x actorPos > minX && x actorPos < maxX)
                   || (y actorPos == maxY && x actorPos > minX && x actorPos < maxX)
               )

isInsideEntity :: (Int, Int) -> Position -> Position -> Bool
isInsideEntity (sizeX, sizeY) entityPos actorPos =
  let (minX, minY) = (x entityPos - 1, y entityPos - 1)
   in let (maxX, maxY) = (x entityPos + sizeX, y entityPos + sizeY)
       in z actorPos == z entityPos
            && x actorPos >= minX
            && x actorPos <= maxX
            && y actorPos >= minY
            && y actorPos <= maxY
