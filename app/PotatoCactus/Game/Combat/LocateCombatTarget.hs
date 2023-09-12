{-# LANGUAGE MultiWayIf #-}

module PotatoCactus.Game.Combat.LocateCombatTarget (LocateTargetArgs (..), locateCombatTarget) where

import PotatoCactus.Game.Combat.AdvanceCombatEntityDeps (AdvanceCombatEntityDeps (..))
import PotatoCactus.Game.Combat.CombatEntity (CombatTarget (..), CombatTargetStatus (InRange, ShouldDisengage, ShouldPathTo))
import PotatoCactus.Game.Entity.Npc.Npc (NpcIndex)
import PotatoCactus.Game.Player (PlayerIndex)
import PotatoCactus.Game.Position (GetPosition (getPosition), Position (..), isWithin)

data LocateTargetArgs = LocateTargetArgs
  { attackRange :: Int,
    deAggroRange :: Int
  }

locateCombatTarget :: AdvanceCombatEntityDeps -> LocateTargetArgs -> Position -> CombatTarget -> CombatTargetStatus
locateCombatTarget deps args _ None = ShouldDisengage
locateCombatTarget deps args actorPos (NpcTarget npcId) =
  case findNpc deps npcId of
    Nothing -> ShouldDisengage
    Just targetPos ->
      locateTargetPos_
        args
        (targetSize_ deps (NpcTarget npcId))
        actorPos
        targetPos
locateCombatTarget deps args actorPos (PlayerTarget playerId) =
  case findPlayer deps playerId of
    Nothing -> ShouldDisengage
    Just targetPos ->
      locateTargetPos_
        args
        (targetSize_ deps (PlayerTarget playerId))
        actorPos
        targetPos

locateTargetPos_ :: LocateTargetArgs -> (Int, Int) -> Position -> Position -> CombatTargetStatus
locateTargetPos_ args targetSize actorPos targetPos =
  let distance = manhattanDistance_ actorPos targetPos
   in if
          | z actorPos /= z targetPos -> ShouldDisengage
          | distance >= deAggroRange args -> ShouldDisengage
          | isInside_ targetSize targetPos actorPos -> ShouldPathTo
          | distance <= attackRange args -> InRange
          | otherwise -> ShouldPathTo

manhattanDistance_ :: Position -> Position -> Int
manhattanDistance_ a b =
  abs (x a - x b) + abs (y a - y b)

targetSize_ :: AdvanceCombatEntityDeps -> CombatTarget -> (Int, Int)
targetSize_ deps (NpcTarget npcId) = npcSize deps npcId
targetSize_ _ _ = (1, 1)

isInside_ :: (Int, Int) -> Position -> Position -> Bool
isInside_ (targetSizeX, targetSizeY) targetPos actorPos =
  x actorPos >= x targetPos
    && x actorPos < x targetPos + targetSizeX
    && y actorPos >= y targetPos
    && y actorPos < y targetPos + targetSizeY
