module PotatoCactus.Game.World.Selectors where

import PotatoCactus.Config.Constants (entityViewingDistance)
import PotatoCactus.Game.Player (Player (serverIndex))
import PotatoCactus.Game.Position (GetPosition (getPosition), Position, isWithin)
import PotatoCactus.Game.World (World (players))
import PotatoCactus.Game.World.MobList (findAllByPredicate)

localEntities :: (GetPosition a) => a -> [a] -> [a]
localEntities ref = filter (isViewableFrom_ ref)

isViewableFrom_ :: (GetPosition a) => a -> a -> Bool
isViewableFrom_ a b = isWithin entityViewingDistance (getPosition a) (getPosition b)

localPlayers :: World -> Player -> [Player]
localPlayers world refPlayer =
  filter
    (\p -> serverIndex p /= serverIndex refPlayer)
    $ findAllByPredicate (players world) (isViewableFrom_ refPlayer)
