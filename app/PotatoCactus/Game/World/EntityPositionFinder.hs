module PotatoCactus.Game.World.EntityPositionFinder where

import PotatoCactus.Game.Combat.CombatEntity (CombatTarget (None, NpcTarget, PlayerTarget))
import PotatoCactus.Game.Entity.Npc.Npc (Npc)
import PotatoCactus.Game.Player (Player (Player))
import PotatoCactus.Game.Position (GetPosition (getPosition), Position)
import PotatoCactus.Game.World.MobList (MobList, findByIndex)

type CombatTargetPosOrDefault = CombatTarget -> Position -> Position
combatTargetPosOrDefault :: MobList Player -> MobList Npc -> CombatTarget -> Position -> Position
combatTargetPosOrDefault _ _ None defaultValue = defaultValue
combatTargetPosOrDefault allPlayers _ (PlayerTarget playerId) defaultValue =
  maybe
    defaultValue
    getPosition
    (findByIndex allPlayers playerId)
combatTargetPosOrDefault _ allNpcs (NpcTarget npcId) defaultValue =
  maybe
    defaultValue
    getPosition
    (findByIndex allNpcs npcId)
