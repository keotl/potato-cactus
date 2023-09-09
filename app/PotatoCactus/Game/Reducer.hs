module PotatoCactus.Game.Reducer where

import qualified PotatoCactus.Boot.GameChannel as C
import qualified PotatoCactus.Game.Entity.GroundItem.GroundItem as GroundItem
import qualified PotatoCactus.Game.Entity.GroundItem.GroundItemCollection as GroundItemCollection
import PotatoCactus.Game.Entity.Interaction.Target (NpcInteractionType (NpcAction, NpcAttack))
import PotatoCactus.Game.Entity.Object.GameObject (GameObject (GameObject))
import PotatoCactus.Game.Interface.InterfaceButtonDispatch (dispatchInterfaceButtonClick)
import PotatoCactus.Game.Message.GameChannelMessage (GameChannelMessage (..))
import PotatoCactus.Game.Message.ItemOnObjectPayload (ItemOnObjectPayload (ItemOnObjectPayload))
import PotatoCactus.Game.Message.ObjectClickPayload (ObjectClickPayload (ObjectClickPayload))
import qualified PotatoCactus.Game.Message.RegisterClientPayload as C
import PotatoCactus.Game.Movement.PositionXY (fromXY)
import qualified PotatoCactus.Game.Movement.PositionXY as Position
import qualified PotatoCactus.Game.Player as P
import PotatoCactus.Game.PlayerUpdate.PlayerUpdate (PlayerUpdate (ContinueDialogue, DropItem, EquipItem, InteractWithGroundItem, InteractWithNpc, InteractWithObject, InteractWithObjectWithItem, SayChatMessage, UnequipItem))
import PotatoCactus.Game.Position (GetPosition (getPosition), Position (z))
import qualified PotatoCactus.Game.Position as Position
import PotatoCactus.Game.Scripting.Actions.CreateInterface (InterfaceType (Standard))
import PotatoCactus.Game.Scripting.ScriptUpdates (GameEvent (DropItemEvent, PlayerCommandEvent))
import PotatoCactus.Game.Typing (advance)
import PotatoCactus.Game.World (ClientHandle (username), World (World, clients, groundItems, players, tick), addPlayer, queueEvent, removePlayerByUsername, updatePlayer, updatePlayerByIndex)
import qualified PotatoCactus.Game.World as W
import PotatoCactus.Game.World.MobList (findByIndex)
import qualified PotatoCactus.Game.World.Selectors as Selectors

reduceWorld :: World -> GameChannelMessage -> World
reduceWorld world (RegisterClientMessage message) =
  (addPlayer world (C.player message)) {clients = C.clientHandle message : clients world}
reduceWorld world (UnregisterClientMessage clientIdentifier) =
  let newClients = filter (unregisterClientPredicate_ clientIdentifier) $ clients world
   in (removePlayerByUsername world clientIdentifier) {clients = newClients}
reduceWorld world (PlayerWalkMessage playerName startPos isRunning steps) = updatePlayer world playerName (P.issueWalkCommand (startPos, isRunning, steps))
reduceWorld world (InterfaceButtonClickMessage playerName button) =
  dispatchInterfaceButtonClick world playerName button
reduceWorld world (PlayerChatMessage playerName message) =
  updatePlayer world playerName (\p -> P.queueUpdate p (SayChatMessage message))
reduceWorld world (EquipItemMessage playerName payload) =
  updatePlayer world playerName (\p -> P.queueUpdate p (EquipItem payload))
reduceWorld world (UnequipItemMessage playerName slot) =
  updatePlayer world playerName (\p -> P.queueUpdate p (UnequipItem slot))
reduceWorld world (ObjectClickMessage playerId objectId positionXY actionIndex) =
  updatePlayerByIndex
    world
    playerId
    ( \p ->
        case W.findObjectAt world (fromXY positionXY (z . getPosition $ p)) objectId of
          Nothing -> p
          Just obj -> P.queueUpdate p (InteractWithObject (ObjectClickPayload obj actionIndex))
    )
reduceWorld world (NpcAttackMessage playerId npcIndex) =
  updatePlayerByIndex world playerId (\p -> P.queueUpdate p (InteractWithNpc npcIndex NpcAttack))
reduceWorld world (NpcClickMessage playerId npcIndex actionIndex) =
  updatePlayerByIndex world playerId (\p -> P.queueUpdate p (InteractWithNpc npcIndex (NpcAction actionIndex)))
reduceWorld world (PickupGroundItemMessage playerId itemId pos) =
  let player = findByIndex (players world) playerId
   in case player of
        Nothing -> world
        Just p ->
          let position = Position.fromXY pos (Position.z . getPosition $ p)
           in case GroundItemCollection.findMatchingItem (itemId, position, P.username p) (groundItems world) of
                Nothing -> world
                Just groundItem ->
                  updatePlayerByIndex
                    world
                    playerId
                    (`P.queueUpdate` InteractWithGroundItem itemId (GroundItem.quantity groundItem) position)
reduceWorld world (PlayerCommandMessage playerId cmd args) =
  queueEvent world $ PlayerCommandEvent playerId cmd args
reduceWorld world (PlayerContinueDialogueMessage playerId _) =
  updatePlayerByIndex world playerId (`P.queueUpdate` ContinueDialogue)
reduceWorld world (ItemOnObjectMessage playerId itemInterfaceId objectId positionXY itemIndexId itemId) =
  updatePlayerByIndex
    world
    playerId
    ( \p ->
        case W.findObjectAt world (fromXY positionXY (z . getPosition $ p)) objectId of
          Nothing -> p
          Just obj ->
            P.queueUpdate
              p
              ( InteractWithObjectWithItem $
                  ItemOnObjectPayload itemInterfaceId obj itemIndexId itemId
              )
    )
reduceWorld world (DropItemMessage playerId widgetId itemId index) =
  updatePlayerByIndex world playerId (`P.queueUpdate` DropItem widgetId itemId index)
reduceWorld world UpdateWorldMessage =
  advance world

unregisterClientPredicate_ :: String -> ClientHandle -> Bool
unregisterClientPredicate_ removed clientHandle =
  removed /= username clientHandle
