module PotatoCactus.Game.Combat.CombatEntity where

import PotatoCactus.Game.Combat.Hit (Hit (damage))
import PotatoCactus.Game.Position (Position)
import PotatoCactus.Game.Typing (Advance (advance))

data CombatTarget = PlayerTarget Int | NpcTarget Int | None deriving (Show, Eq)

data CombatState = Alive | Dying | Dead deriving (Show, Eq)

data CombatTargetStatus = InRange | ShouldPathTo Position | ShouldDisengage deriving (Show, Eq)

data CombatAction = MoveTowardsTarget Position | AttackTarget deriving (Show, Eq)
-- TODO - Return list of possible canditate positions for pathing  - keotl 2023-09-18
-- TODO - Pick one at random on tick advance  - keotl 2023-09-18
-- TODO - For NPCs, do not move from under target if the randomly picked direction is inaccessible.  - keotl 2023-09-18

data CombatEntity = CombatEntity
  { hitpoints :: Int,
    state :: CombatState,
    maxHitpoints :: Int,
    target :: CombatTarget,
    hits :: [Hit],
    cooldown :: Int,
    pendingActions :: [CombatAction]
  }
  deriving (Show)

create :: Int -> CombatEntity
create hitpoints =
  CombatEntity
    { hitpoints = hitpoints,
      state = Alive,
      maxHitpoints = hitpoints,
      target = None,
      hits = [],
      cooldown = 0,
      pendingActions = []
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

clearTarget :: CombatEntity -> CombatEntity
clearTarget c = c {target = None}

setAttackCooldown :: CombatEntity -> Int -> CombatEntity
setAttackCooldown c cooldown =
  c {cooldown = cooldown}

setTarget :: CombatEntity -> CombatTarget -> CombatEntity
setTarget c target =
  c {target = target}
