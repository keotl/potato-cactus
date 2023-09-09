module PotatoCactus.Game.PlayerUpdate.ProcessPlayerUpdate where

import Data.Bits ((.|.))
import PotatoCactus.Game.Definitions.EquipmentDefinitions (EquipmentDefinition (slot), equipmentDefinition)
import PotatoCactus.Game.Entity.Interaction.Interaction (createForTarget)
import qualified PotatoCactus.Game.Entity.Interaction.Target as ITarget
import qualified PotatoCactus.Game.Entity.Object.GameObject as Obj
import PotatoCactus.Game.Interface.InterfaceController (clearStandardInterfaces)
import qualified PotatoCactus.Game.Interface.InterfaceController as IC
import PotatoCactus.Game.ItemContainer (ItemStack (Empty, ItemStack, itemId), StackPolicy (Standard), addItems, atIndex, canAddItems, replaceStack)
import qualified PotatoCactus.Game.ItemContainer as IContainer
import PotatoCactus.Game.Message.EquipItemMessagePayload (EquipItemMessagePayload (EquipItemMessagePayload, itemIndex))
import qualified PotatoCactus.Game.Message.ItemOnObjectPayload as IonO
import qualified PotatoCactus.Game.Message.ItemOnObjectPayload as ItemOnObject
import qualified PotatoCactus.Game.Message.ObjectClickPayload as ObjectClick
import PotatoCactus.Game.Movement.PositionXY (fromXY)
import PotatoCactus.Game.Player (Player (chatMessage, droppedItemIndices, equipment, interaction, interfaces, inventory, updateMask))
import PotatoCactus.Game.PlayerUpdate.Equipment (Equipment (container), equipItem, unequipItem)
import PotatoCactus.Game.PlayerUpdate.PlayerUpdate (PlayerUpdate (ContinueDialogue, DropItem, EquipItem, InteractWithGroundItem, InteractWithNpc, InteractWithObject, InteractWithObjectWithItem, SayChatMessage, UnequipItem))
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
processPlayerUpdate
  p
  ( InteractWithObject
      ObjectClick.ObjectClickPayload
        { ObjectClick.object = object,
          ObjectClick.actionIndex = actionIndex
        }
    ) =
    p
      { interaction =
          createForTarget
            ( ITarget.ObjectTarget
                object
                (ITarget.ObjectAction actionIndex)
            ),
        interfaces = clearStandardInterfaces . interfaces $ p
      }
processPlayerUpdate
  p
  ( InteractWithObjectWithItem
      ItemOnObject.ItemOnObjectPayload
        { ItemOnObject.object = object,
          ItemOnObject.interfaceId = interfaceId,
          ItemOnObject.itemIndex = itemIndex,
          ItemOnObject.itemId = itemId
        }
    ) =
    if IContainer.isItem itemId (IContainer.atIndex itemIndex (inventory p))
      then
        p
          { interaction =
              createForTarget
                ( ITarget.ObjectTarget
                    object
                    (ITarget.ItemOnObject interfaceId itemIndex itemId)
                ),
            interfaces = clearStandardInterfaces . interfaces $ p
          }
      else p
processPlayerUpdate p (InteractWithNpc npcId interactionType) =
  p
    { interaction = createForTarget (ITarget.NpcTarget npcId interactionType),
      interfaces = clearStandardInterfaces . interfaces $ p
    }
processPlayerUpdate p (InteractWithGroundItem itemId quantity pos) =
  p
    { interaction = createForTarget (ITarget.GroundItemTarget itemId quantity pos ITarget.ItemPickup),
      interfaces = clearStandardInterfaces . interfaces $ p
    }
processPlayerUpdate p ContinueDialogue =
  p {interfaces = IC.closeInterface (interfaces p) I.Standard}
processPlayerUpdate p (DropItem 3214 itemId index) =
  if IContainer.isItem itemId (IContainer.atIndex index (inventory p))
    && index `notElem` droppedItemIndices p
    then p {droppedItemIndices = index : droppedItemIndices p}
    else p
processPlayerUpdate p _ = p
