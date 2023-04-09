module PotatoCactus.Game.World.Selectors where

import PotatoCactus.Config.Constants (entityViewingDistance)
import PotatoCactus.Game.Entity.Npc.Npc (Npc)
import PotatoCactus.Game.Player (Player (serverIndex))
import PotatoCactus.Game.Position (GetPosition (getPosition), Position, isWithin)
import PotatoCactus.Game.Typing (IsEntityActive (isEntityActive))
import PotatoCactus.Game.World (World (npcs, players))
import PotatoCactus.Game.World.MobList (findAllByPredicate, findByPredicate)

localEntities :: (GetPosition a) => a -> [a] -> [a]
localEntities ref = filter (isViewableFrom_ ref)

isViewableFrom_ :: (GetPosition a, GetPosition b) => a -> b -> Bool
isViewableFrom_ a b = isWithin entityViewingDistance (getPosition a) (getPosition b)

localPlayers :: World -> Player -> [Player]
localPlayers world refPlayer =
  filter
    (\p -> serverIndex p /= serverIndex refPlayer)
    $ findAllByPredicate (players world) (isViewableFrom_ refPlayer)

localNpcs :: World -> Player -> [Npc]
localNpcs world refPlayer =
  findAllByPredicate (npcs world) (\npc -> isViewableFrom_ refPlayer npc && isEntityActive npc)

isNpcAt :: World -> Position -> Bool
isNpcAt w pos =
  case findByPredicate (npcs w) ((==) pos . getPosition) of
    Nothing -> False
    Just _ -> True
