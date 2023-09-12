module PotatoCactus.Game.Entity.Npc.AdvanceNpc (advanceNpc) where

import PotatoCactus.Config.Constants (npcDisengageDistance)
import PotatoCactus.Game.Combat.AdvanceCombatEntity (advanceCombatEntity)
import PotatoCactus.Game.Combat.AdvanceCombatEntityDeps (AdvanceCombatEntityDeps)
import PotatoCactus.Game.Combat.CombatEntity (CombatEntity (target), CombatTarget (None, NpcTarget, PlayerTarget), clearTarget)
import PotatoCactus.Game.Combat.LocateCombatTarget (LocateTargetArgs (LocateTargetArgs), locateCombatTarget)
import PotatoCactus.Game.Definitions.NpcDefinitions (NpcDefinition (attackRange), npcDefinition)
import PotatoCactus.Game.Entity.Npc.Npc (Npc (..))
import qualified PotatoCactus.Game.Entity.Npc.Npc as NPC
import PotatoCactus.Game.Entity.Npc.NpcMovement (create, immediatelyQueueMovement)
import PotatoCactus.Game.Entity.Npc.RespawnStrategy (restart, tryRespawn)
import PotatoCactus.Game.Movement.PathPlanner (CollisionMap, findPath)
import PotatoCactus.Game.Position (GetPosition (getPosition), Position (Position), faraway, isAdjacent, isWithin)
import PotatoCactus.Game.Typing (IsEntityActive (isEntityActive), advance)
import PotatoCactus.Game.World.EntityPositionFinder (CombatTargetPosOrDefault)
import PotatoCactus.Game.World.MobList (findByIndex)
import PotatoCactus.Utils.Flow ((|>))

advanceNpc :: AdvanceCombatEntityDeps -> Npc -> Npc
advanceNpc deps npc =
  if not . isEntityActive $ npc
    then advanceRespawnProcess npc
    else
      npc
        |> clearTransientProperties_
        |> advanceAttributes_ deps

clearTransientProperties_ :: Npc -> Npc
clearTransientProperties_ npc =
  npc
    { updateMask = 0,
      animation = Nothing,
      forcedChat = Nothing
    }

advanceAttributes_ :: AdvanceCombatEntityDeps -> Npc -> Npc
advanceAttributes_ deps npc =
  npc
    { movement = advance . movement $ npc,
      combat =
        advanceCombatEntity
          (locateCombatTarget deps (LocateTargetArgs 1 npcDisengageDistance) (getPosition npc))
          (combat npc)
    }

advanceRespawnProcess :: Npc -> Npc
advanceRespawnProcess npc =
  case tryRespawn . respawn $ npc of
    Just pos ->
      ( NPC.create
          (definitionId npc)
          pos
          (restart . respawn $ npc)
      )
        { serverIndex = serverIndex npc
        }
    Nothing -> npc {respawn = advance . respawn $ npc}
