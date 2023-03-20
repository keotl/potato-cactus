module PotatoCactus.Game.Entity.Npc.AdvanceNpc (advanceNpc) where

import PotatoCactus.Config.Constants (npcDisengageDistance)
import PotatoCactus.Game.Combat.CombatEntity (CombatEntity (target), CombatTarget (None, NpcTarget, PlayerTarget), clearTarget)
import PotatoCactus.Game.Definitions.NpcDefinitions (NpcDefinition (attackRange), npcDefinition)
import PotatoCactus.Game.Entity.Npc.Npc (Npc (..))
import PotatoCactus.Game.Entity.Npc.NpcMovement (create, immediatelyQueueMovement)
import PotatoCactus.Game.Movement.PathPlanner (CollisionMap, findPath)
import PotatoCactus.Game.Position (GetPosition (getPosition), Position (Position), faraway, isWithin)
import PotatoCactus.Game.Typing (advance)
import PotatoCactus.Game.World.EntityPositionFinder (CombatTargetPosOrDefault)
import PotatoCactus.Game.World.MobList (findByIndex)

advanceNpc :: CombatTargetPosOrDefault -> CollisionMap -> Npc -> Npc
advanceNpc targetPosOrDefault collisionMap npc =
  if canReachCombatTarget_ npc targetPosOrDefault
    then
      npc
        { movement = advance . movement $ npc,
          combat = advance . combat $ npc,
          updateMask = 0
        }
    else
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
                  updateMask = 0
                }
            else
              npc
                { movement =
                    immediatelyQueueMovement
                      (movement npc)
                      ( findPath
                          collisionMap
                          (getPosition npc)
                          (targetPosOrDefault (target . combat $ npc) (getPosition npc))
                      ),
                  combat = advance . combat $ npc,
                  updateMask = 0
                }

canReachCombatTarget_ :: Npc -> CombatTargetPosOrDefault -> Bool
canReachCombatTarget_ npc targetPosOrDefault =
  let npcDef = npcDefinition . definitionId $ npc
   in isWithin
        (range_ npcDef)
        (getPosition npc)
        (targetPosOrDefault (target . combat $ npc) (getPosition npc))

range_ :: Maybe NpcDefinition -> Int
range_ Nothing = 1
range_ (Just def) = attackRange def
