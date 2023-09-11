{-# LANGUAGE MultiWayIf #-}

module PotatoCactus.Game.Combat.LocateCombatTarget (LocateTargetArgs (..), locateCombatTarget) where

import PotatoCactus.Game.Combat.CombatEntity (CombatTarget (..), CombatTargetStatus (InRange, ShouldDisengage, ShouldPathTo))
import PotatoCactus.Game.Entity.Npc.Npc (NpcIndex)
import PotatoCactus.Game.Player (PlayerIndex)
import PotatoCactus.Game.Position (GetPosition (getPosition), Position (..), isWithin)

data LocateTargetArgs = LocateTargetArgs
  { attackRange :: Int,
    deAggroRange :: Int,
    findNpc :: NpcIndex -> Maybe Position,
    findPlayer :: PlayerIndex -> Maybe Position
  }

locateCombatTarget :: LocateTargetArgs -> Position -> CombatTarget -> CombatTargetStatus
locateCombatTarget args _ None = ShouldDisengage
locateCombatTarget args actorPos (NpcTarget npcId) =
  case findNpc args npcId of
    Nothing -> ShouldDisengage
    Just targetPos -> locateTargetPos_ args actorPos targetPos
locateCombatTarget args actorPos (PlayerTarget playerId) =
  case findPlayer args playerId of
    Nothing -> ShouldDisengage
    Just targetPos -> locateTargetPos_ args actorPos targetPos

locateTargetPos_ :: LocateTargetArgs -> Position -> Position -> CombatTargetStatus
locateTargetPos_ args actorPos targetPos =
  let distance = manhattanDistance_ actorPos targetPos
   in if
          | z actorPos /= z targetPos -> ShouldDisengage
          | distance >= deAggroRange args -> ShouldDisengage
          | distance <= attackRange args -> InRange
          | otherwise -> ShouldPathTo

manhattanDistance_ :: Position -> Position -> Int
manhattanDistance_ a b =
  abs (x a - x b) + abs (y a - y b)
