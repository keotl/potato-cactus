module PotatoCactus.Game.PlayerUpdate.ProcessPlayerUpdate where

import Data.Bits ((.|.))
import PotatoCactus.Game.Definitions.EquipmentDefinitions (EquipmentDefinition (slot), equipmentDefinition)
import PotatoCactus.Game.Entity.Interaction.Interaction (createForTarget)
import PotatoCactus.Game.Entity.Interaction.Target (GroundItemInteractionType (ItemPickup), InteractionTarget (GroundItemTarget, NpcTarget, ObjectTarget))
import PotatoCactus.Game.Entity.Object.GameObjectKey (GameObjectKey (GameObjectKey))
import PotatoCactus.Game.Interface.InterfaceController (clearStandardInterfaces)
import qualified PotatoCactus.Game.Interface.InterfaceController as IC
import PotatoCactus.Game.ItemContainer (ItemStack (Empty, ItemStack, itemId), StackPolicy (Standard), addItems, atIndex, canAddItems, replaceStack)
import PotatoCactus.Game.Message.EquipItemMessagePayload (EquipItemMessagePayload (EquipItemMessagePayload, itemIndex))
import qualified PotatoCactus.Game.Message.ItemOnObjectPayload as IonO
import PotatoCactus.Game.Message.ObjectClickPayload (ObjectClickPayload (actionIndex))
import PotatoCactus.Game.Movement.PositionXY (fromXY)
import PotatoCactus.Game.Player (Player (chatMessage, equipment, interaction, interfaces, inventory, updateMask))
import PotatoCactus.Game.PlayerUpdate.Equipment (Equipment (container), equipItem, unequipItem)
import PotatoCactus.Game.PlayerUpdate.PlayerUpdate (PlayerUpdate (ContinueDialogue, EquipItem, InteractWithGroundItem, InteractWithNpc, InteractWithObject, InteractWithObjectWithItem, SayChatMessage, UnequipItem))
import PotatoCactus.Game.PlayerUpdate.UpdateMask (appearanceFlag, chatFlag)
import PotatoCactus.Game.Position (GetPosition (getPosition), Position (z))
import qualified PotatoCactus.Game.Scripting.Actions.CreateInterface as I

processPlayerUpdate :: Player -> PlayerUpdate -> Player
processPlayerUpdate p (SayChatMessage message) =
  p {chatMessage = Just message, updateMask = updateMask p .|. chatFlag}
processPlayerUpdate p (EquipItem (EquipItemMessagePayload _ itemIndex 3214)) =
  case atIndex itemIndex (inventory p) of
    Empty -> p
    stack ->
      case equipmentDefinition . itemId $ stack of
        Nothing -> p
        Just def ->
          let (updatedEquipment, replaced) = equipItem (slot def) (equipment p) stack
           in let (inventoryWithRemoved, _) = replaceStack itemIndex (inventory p) Empty
               in if canAddItems inventoryWithRemoved replaced
                    then
                      ( p
                          { inventory = addItems inventoryWithRemoved replaced,
                            equipment = updatedEquipment,
                            updateMask = updateMask p .|. appearanceFlag,
                            interfaces = clearStandardInterfaces . interfaces $ p
                          }
                      )
                    else p
processPlayerUpdate p (UnequipItem slot) =
  let (updatedEquipment, removedItems) = unequipItem slot (equipment p)
   in if canAddItems (inventory p) removedItems
        then
          p
            { inventory = addItems (inventory p) removedItems,
              equipment = updatedEquipment,
              updateMask = updateMask p .|. appearanceFlag,
              interfaces = clearStandardInterfaces . interfaces $ p
            }
        else p
-- processPlayerUpdate p (InteractWithObject payload) =
--   p
--     { interaction =
--         createForTarget
--           ( ObjectTarget
--               (GameObjectKey (objectId payload) (fromXY (position payload) (z . getPosition $ p)))
--               (Left $ index payload)
--           ),
--       interfaces = clearStandardInterfaces . interfaces $ p
--     }
-- processPlayerUpdate p (InteractWithObjectWithItem payload) =
--   p
--     { interaction =
--         createForTarget
--           ( ObjectTarget
--               (GameObjectKey (IonO.objectId payload) (fromXY (IonO.position payload) (z . getPosition $ p)))
--               (Right payload)
--           ),
--       interfaces = clearStandardInterfaces . interfaces $ p
--     }
processPlayerUpdate p (InteractWithNpc npcId interactionType) =
  p
    { interaction = createForTarget (NpcTarget npcId interactionType),
      interfaces = clearStandardInterfaces . interfaces $ p
    }
processPlayerUpdate p (InteractWithGroundItem itemId quantity pos) =
  p
    { interaction = createForTarget (GroundItemTarget itemId quantity pos ItemPickup),
      interfaces = clearStandardInterfaces . interfaces $ p
    }
processPlayerUpdate p ContinueDialogue =
  p {interfaces = IC.closeInterface (interfaces p) I.Standard}
processPlayerUpdate p _ = p
