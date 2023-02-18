module PotatoCactus.Game.World.Selectors where

import PotatoCactus.Config.Constants (entityViewingDistance)
import PotatoCactus.Game.Position (GetPosition (getPosition), Position, isWithin)

localEntities :: (GetPosition a) => a -> [a] -> [a]
localEntities ref = filter (isViewableFrom_ ref)

isViewableFrom_ :: (GetPosition a) => a -> a -> Bool
isViewableFrom_ a b = isWithin entityViewingDistance (getPosition a) (getPosition b)
