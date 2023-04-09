module PotatoCactus.Game.Entity.Npc.RespawnStrategy (RespawnStrategy (Never), respawning, shouldDiscardEntity, markEntityDead) where

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
  Respawn_ pos delay 0
instance Show RespawnStrategy where
  show Respawn_ {} = "Respawn"
  show Never = "Never"

doRespawn :: RespawnStrategy -> Maybe Position
doRespawn Respawn_ {spawnCooldown = 0, spawnPoint = point} = Just point
doRespawn _ = Nothing

shouldDiscardEntity :: RespawnStrategy -> Bool
shouldDiscardEntity Never = True
shouldDiscardEntity _ = False

markEntityDead :: RespawnStrategy -> RespawnStrategy
markEntityDead Never = Never
markEntityDead r = r {spawnCooldown = spawnDelay r}

instance Advance RespawnStrategy where
  advance Never = Never
  advance r = r {spawnCooldown = max 0 (spawnCooldown r - 1)}
