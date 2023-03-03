module PotatoCactus.Game.PlayerUpdate.Equipment where

import Data.Maybe (mapMaybe)
import PotatoCactus.Game.Definitions.EquipmentDefinitions (EquipmentDefinition (shouldHideModelWhenEquipped), EquipmentSlot, equipmentDefinition)
import PotatoCactus.Game.ItemContainer (ItemContainer (content), ItemStack (Empty, ItemStack), atIndex, replaceStack)
import PotatoCactus.Game.Typing (Advance (advance))
import PotatoCactus.Utils.Iterable (replaceAtIndex)
import Prelude hiding (id)

data Equipment = Equipment {container :: ItemContainer} deriving (Show)

instance Advance Equipment where
  advance (Equipment container) = Equipment (advance container)

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
    ItemStack itemDef _ -> itemDef

shouldShowModel :: EquipmentSlot -> Equipment -> Bool
shouldShowModel slot equipment =
  case atIndex slot (container equipment) of
    Empty -> True
    ItemStack item _ ->
      case equipmentDefinition item of
        Just def -> not . shouldHideModelWhenEquipped $ def
        Nothing -> True
