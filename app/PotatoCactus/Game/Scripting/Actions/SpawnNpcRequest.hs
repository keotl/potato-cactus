module PotatoCactus.Game.Scripting.Actions.SpawnNpcRequest where

import PotatoCactus.Game.Definitions.NpcDefinitions (NpcDefinitionId)
import PotatoCactus.Game.Position (Position (Position))

data SpawnNpcRequest = SpawnNpcRequest
  { npcId :: NpcDefinitionId,
    position :: Position,
    respawnDelay :: Int
  } deriving(Show)
