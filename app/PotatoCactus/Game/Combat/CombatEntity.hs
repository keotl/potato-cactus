module PotatoCactus.Game.Combat.CombatEntity where

import PotatoCactus.Game.Combat.Hit (Hit (damage))
import PotatoCactus.Game.Position (Position)
import PotatoCactus.Game.Typing (Advance (advance))

data CombatTarget = PlayerTarget Int | NpcTarget Int | None deriving (Show, Eq)

data CombatState = Alive | Dying | Dead deriving (Show)

data CombatEntity = CombatEntity
  { hitpoints :: Int,
    state :: CombatState,
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
      state = Alive,
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
    case state c of
      Dying -> c {state = Dead}
      Dead -> c
      Alive ->
        c
          { hits = [],
            cooldown = max 0 (cooldown c - 1),
            state = if hitpoints c == 0 then Dying else Alive
          }

clearTarget :: CombatEntity -> CombatEntity
clearTarget c = c {target = None}

setAttackCooldown :: CombatEntity -> Int -> CombatEntity
setAttackCooldown c cooldown =
  c {cooldown = cooldown}

setTarget :: CombatEntity -> CombatTarget -> CombatEntity
setTarget c target =
  c {target = target}

clearTargetIfEngagedWith :: CombatTarget -> CombatEntity -> CombatEntity
clearTargetIfEngagedWith desiredTarget c =
  if target c == desiredTarget then clearTarget c else c
