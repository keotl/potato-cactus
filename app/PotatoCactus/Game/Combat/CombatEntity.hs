module PotatoCactus.Game.Combat.CombatEntity where

import PotatoCactus.Game.Combat.Hit (Hit (damage))
import PotatoCactus.Game.Typing (Advance (advance))
import PotatoCactus.Game.Position (Position)

data CombatTarget = PlayerTarget Int | NpcTarget Int | None deriving (Show)

data CombatEntity = CombatEntity
  { hitpoints :: Int,
    maxHitpoints :: Int,
    target :: CombatTarget,
    hits :: [Hit],
    cooldown :: Int
  }
  deriving (Show)

create :: Int -> CombatEntity
create hitpoints =
  CombatEntity
    { hitpoints = hitpoints,
      maxHitpoints = hitpoints,
      target = None,
      hits = [],
      cooldown = 0
    }

applyHit :: CombatEntity -> CombatTarget -> Hit -> CombatEntity
applyHit c damageSource hit =
  c
    { hitpoints = max 0 (hitpoints c - damage hit),
      hits = hit : hits c,
      target = case target c of
        None -> damageSource
        _ -> target c -- TODO - Retaliation logic  - keotl 2023-03-20
    }

instance Advance CombatEntity where
  advance c =
    c
      { hits = [],
        cooldown = max 0 (cooldown c - 1)
      }

clearTarget :: CombatEntity -> CombatEntity
clearTarget c = c {target = None}

setAttackCooldown :: CombatEntity -> Int -> CombatEntity
setAttackCooldown c cooldown =
  c {cooldown = cooldown}

setTarget :: CombatEntity -> CombatTarget -> CombatEntity
setTarget c target =
  c {target = target}
