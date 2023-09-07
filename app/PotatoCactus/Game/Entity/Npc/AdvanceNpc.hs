module PotatoCactus.Game.Entity.Npc.AdvanceNpc (advanceNpc) where

import PotatoCactus.Config.Constants (npcDisengageDistance)
import PotatoCactus.Game.Combat.CombatEntity (CombatEntity (target), CombatTarget (None, NpcTarget, PlayerTarget), clearTarget)
import PotatoCactus.Game.Definitions.NpcDefinitions (NpcDefinition (attackRange), npcDefinition)
import PotatoCactus.Game.Entity.Npc.Npc (Npc (..))
import qualified PotatoCactus.Game.Entity.Npc.Npc as NPC
import PotatoCactus.Game.Entity.Npc.NpcMovement (create, immediatelyQueueMovement)
import PotatoCactus.Game.Entity.Npc.RespawnStrategy (restart, tryRespawn)
import PotatoCactus.Game.Movement.PathPlanner (CollisionMap, findPath)
import PotatoCactus.Game.Position (GetPosition (getPosition), Position (Position), faraway, isWithin, isAdjacent)
import PotatoCactus.Game.Typing (IsEntityActive (isEntityActive), advance)
import PotatoCactus.Game.World.EntityPositionFinder (CombatTargetPosOrDefault)
import PotatoCactus.Game.World.MobList (findByIndex)

advanceNpc :: CombatTargetPosOrDefault -> Npc -> Npc
advanceNpc targetPosOrDefault npc =
  if not . isEntityActive $ npc
    then advanceRespawnProcess npc
    else
      let withCombat = updateCombat targetPosOrDefault npc
       in withCombat
            { -- Clear transient properties
              updateMask = 0,
              animation = Nothing,
              forcedChat = Nothing
            }

updateCombat :: CombatTargetPosOrDefault -> Npc -> Npc
updateCombat targetPosOrDefault npc =
  let shouldDisengage =
        not $
          isWithin
            npcDisengageDistance
            (targetPosOrDefault (target . combat $ npc) (faraway . getPosition $ npc))
            (getPosition npc)
   in if shouldDisengage
        then
          npc
            { movement = create . getPosition $ npc,
              combat = clearTarget . combat $ npc,
              canReachTarget = True
            }
        else
          npc
            { movement = advance . movement $ npc,
              combat = advance . combat $ npc,
              canReachTarget = canReachCombatTarget_ npc targetPosOrDefault
            }

canReachCombatTarget_ :: Npc -> CombatTargetPosOrDefault -> Bool
canReachCombatTarget_ npc targetPosOrDefault =
  let npcDef = npcDefinition . definitionId $ npc
   in if range_ npcDef > 1
        then
          isWithin
            (range_ npcDef)
            (getPosition npc)
            (targetPosOrDefault (target . combat $ npc) (getPosition npc))
        else
          isAdjacent
            (getPosition npc)
            (targetPosOrDefault (target . combat $ npc) (getPosition npc))

range_ :: Maybe NpcDefinition -> Int
range_ Nothing = 1
range_ (Just def) = attackRange def

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
