module PotatoCactus.Game.PlayerUpdate.Equipment where

import PotatoCactus.Game.ItemContainer (ItemContainer (content), ItemStack, replaceStack)
import PotatoCactus.Utils.Iterable (replaceAtIndex)

data Equipment = Equipment {container :: ItemContainer} deriving (Show)

type EquipmentSlot = Int

headSlot = 0

capeSlot = 1

amuletSlot = 2

weaponSlot = 3

chestSlot = 4

shieldSlot = 5

legsSlot = 7

handsSlot = 9

feetSlot = 10

ringSlot = 12

ammoSlot = 13

equipItem :: EquipmentSlot -> Equipment -> ItemStack -> (Equipment, ItemStack)
equipItem slot (Equipment container) item =
  let (newContainer, replaced) = replaceStack slot container item
   in (Equipment newContainer, replaced)
