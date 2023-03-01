module PotatoCactus.Game.PlayerUpdate.Equipment where

import PotatoCactus.Game.Item (Item (equippedShouldHideModel_), id)
import PotatoCactus.Game.ItemContainer (ItemContainer (content), ItemStack (Empty, ItemStack), atIndex, replaceStack)
import PotatoCactus.Game.Typing (Advance (advance))
import PotatoCactus.Utils.Iterable (replaceAtIndex)
import Prelude hiding (id)

data Equipment = Equipment {container :: ItemContainer} deriving (Show)

instance Advance Equipment where
  advance (Equipment container) = Equipment (advance container)

type EquipmentSlot = Int

headSlot :: EquipmentSlot
headSlot = 0

capeSlot :: EquipmentSlot
capeSlot = 1

amuletSlot :: EquipmentSlot
amuletSlot = 2

weaponSlot :: EquipmentSlot
weaponSlot = 3

chestSlot :: EquipmentSlot
chestSlot = 4

shieldSlot :: EquipmentSlot
shieldSlot = 5

legsSlot :: EquipmentSlot
legsSlot = 7

handsSlot :: EquipmentSlot
handsSlot = 9

feetSlot :: EquipmentSlot
feetSlot = 10

ringSlot :: EquipmentSlot
ringSlot = 12

ammoSlot :: EquipmentSlot
ammoSlot = 13

equipItem :: EquipmentSlot -> Equipment -> ItemStack -> (Equipment, [ItemStack])
equipItem slot (Equipment container) item =
  -- TODO - Case where we have to remove multiple items at once. e.g. 2H sword  - keotl 2023-03-01
  let (newContainer, replaced) = replaceStack slot container item
   in (Equipment newContainer, [replaced])

unequipItem :: EquipmentSlot -> Equipment -> (Equipment, [ItemStack])
unequipItem slot equipment =
  equipItem slot equipment Empty

itemIdAtSlot :: EquipmentSlot -> Equipment -> Int
itemIdAtSlot slot (Equipment container) =
  case atIndex slot container of
    Empty -> 0
    ItemStack itemDef _ -> id itemDef

shouldShowModel :: EquipmentSlot -> Equipment -> Bool
shouldShowModel slot equipment =
  case atIndex slot (container equipment) of
    Empty -> True
    ItemStack item _ -> not . equippedShouldHideModel_ $ item
