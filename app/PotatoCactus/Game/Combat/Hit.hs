module PotatoCactus.Game.Combat.Hit where

import Data.Binary (Word8)

data DamageType = MeleeAttack | RangedAttack | MagicAttack | Poison | Disease deriving (Show)

data Hit = Hit
  { damage :: Int,
    damageType :: DamageType
  }
  deriving (Show)

hitsplatOpcode :: Hit -> Word8
hitsplatOpcode (Hit 0 _) = 0
hitsplatOpcode (Hit _ Poison) = 2
hitsplatOpcode (Hit _ Disease) = 3
hitsplatOpcode _ = 1
