module PotatoCactus.Game.PlayerUpdate.ProcessPlayerUpdate where

import Data.Bits ((.|.))
import Debug.Trace (trace)
import PotatoCactus.Game.Item (Item (equipSlot))
import PotatoCactus.Game.ItemContainer (ItemStack (Empty, ItemStack, item), addItems, atIndex, canAddItems, replaceStack)
import PotatoCactus.Game.Message.EquipItemMessagePayload (EquipItemMessagePayload (EquipItemMessagePayload, itemIndex))
import PotatoCactus.Game.Player (Player (chatMessage, equipment, inventory, updateMask))
import PotatoCactus.Game.PlayerUpdate.Equipment (Equipment (container), equipItem, unequipItem)
import PotatoCactus.Game.PlayerUpdate.PlayerUpdate (PlayerUpdate (EquipItem, SayChatMessage, UnequipItem))
import PotatoCactus.Game.PlayerUpdate.UpdateMask (appearanceFlag, chatFlag)

processPlayerUpdate :: Player -> PlayerUpdate -> Player
processPlayerUpdate p (SayChatMessage message) =
  p {chatMessage = Just message, updateMask = updateMask p .|. chatFlag}
processPlayerUpdate p (EquipItem (EquipItemMessagePayload _ itemIndex 3214)) =
  case atIndex itemIndex (inventory p) of
    Empty -> p
    stack ->
      if equipSlot (item stack) /= -1
        then
          let (updatedEquipment, replaced) = equipItem (equipSlot (item stack)) (equipment p) stack
           in let (inventoryWithRemoved, _) = replaceStack itemIndex (inventory p) Empty
               in if canAddItems inventoryWithRemoved replaced
                    then
                      ( p
                          { inventory = addItems inventoryWithRemoved replaced,
                            equipment = updatedEquipment,
                            updateMask = updateMask p .|. appearanceFlag
                          }
                      )
                    else p
        else p
processPlayerUpdate p (UnequipItem slot) =
  let (updatedEquipment, removedItems) = unequipItem slot (equipment p)
   in if canAddItems (inventory p) removedItems
        then
          p
            { inventory = addItems (inventory p) removedItems,
              equipment = updatedEquipment,
              updateMask = updateMask p .|. appearanceFlag
            }
        else p
processPlayerUpdate p _ = p
