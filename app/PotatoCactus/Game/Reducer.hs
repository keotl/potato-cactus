module PotatoCactus.Game.Reducer where

import qualified PotatoCactus.Boot.GameChannel as C
import PotatoCactus.Game.Entity.Object.DispatchGameObjectClick (dispatchGameObjectClick)
import PotatoCactus.Game.Entity.Object.GameObject (GameObject (GameObject))
import PotatoCactus.Game.Interface.InterfaceButtonDispatch (dispatchInterfaceButtonClick)
import PotatoCactus.Game.Message.GameChannelMessage (GameChannelMessage (..))
import PotatoCactus.Game.Message.ObjectClickPayload (ObjectClickPayload (objectId))
import qualified PotatoCactus.Game.Message.RegisterClientPayload as C
import qualified PotatoCactus.Game.Player as P
import PotatoCactus.Game.PlayerUpdate.PlayerUpdate (PlayerUpdate (EquipItem, InteractWithObject, SayChatMessage, UnequipItem))
import PotatoCactus.Game.Typing (advance)
import PotatoCactus.Game.World (ClientHandle (username), World (World, clickedEntity, clients, players, queuedEntityClick_, tick), addPlayer, removePlayerByUsername, updatePlayer, updatePlayerByIndex)

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
reduceWorld world (ObjectClickMessage playerId payload) =
  updatePlayerByIndex world playerId (\p -> P.queueUpdate p (InteractWithObject payload))
reduceWorld world UpdateWorldMessage =
  advance world

unregisterClientPredicate_ :: String -> ClientHandle -> Bool
unregisterClientPredicate_ removed clientHandle =
  removed /= username clientHandle
