module PotatoCactus.Game.Entity.Npc.NpcUpdateMask where

import Data.Binary (Word16)

type NpcUpdateMask = Word16

npcPrimaryHealthUpdateFlag :: Word16
npcPrimaryHealthUpdateFlag = 64

npcSecondaryHealthUpdateFlag :: Word16
npcSecondaryHealthUpdateFlag = 8

npcAnimationUpdateFlag :: Word16
npcAnimationUpdateFlag = 16
