module PotatoCactus.Game.Entity.Npc.RespawnStrategy (RespawnStrategy (Never), respawning, shouldDiscardEntity, tryRespawn, restart) where

import PotatoCactus.Game.Position (Position)
import PotatoCactus.Game.Typing (Advance (advance))

data RespawnStrategy
  = Respawn_
      { spawnPoint :: Position,
        spawnDelay :: Int,
        spawnCooldown :: Int
      }
  | Never

respawning :: Position -> Int -> RespawnStrategy
respawning pos delay =
  Respawn_ pos delay delay

instance Show RespawnStrategy where
  show Respawn_ {} = "Respawn"
  show Never = "Never"

tryRespawn :: RespawnStrategy -> Maybe Position
tryRespawn Respawn_ {spawnCooldown = 0, spawnPoint = point} = Just point
tryRespawn _ = Nothing

shouldDiscardEntity :: RespawnStrategy -> Bool
shouldDiscardEntity Never = True
shouldDiscardEntity _ = False

instance Advance RespawnStrategy where
  advance Never = Never
  advance r = r {spawnCooldown = max 0 (spawnCooldown r - 1)}

restart :: RespawnStrategy -> RespawnStrategy
restart Never = Never
restart (Respawn_ pos delay _) = Respawn_ pos delay delay
