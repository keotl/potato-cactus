module PotatoCactus.Game.Reducer where

import PotatoCactus.Boot.GameChannel (GameChannelMessage (InterfaceButtonClickMessage, PlayerChatMessage, PlayerWalkMessage, RegisterClientMessage, UnregisterClientMessage, UpdateWorldMessage), RegisterClientPayload (clientHandle))
import qualified PotatoCactus.Boot.GameChannel as C
import PotatoCactus.Game.Interface.InterfaceButtonDispatch (dispatchInterfaceButtonClick)
import qualified PotatoCactus.Game.Player as P
import PotatoCactus.Game.PlayerUpdate.PlayerUpdate (PlayerUpdate (SayChatMessage))
import PotatoCactus.Game.Typing (advance)
import PotatoCactus.Game.World (ClientHandle (username), World (World, clients, players, tick), addPlayer, removePlayerByUsername, updatePlayer)

reduceWorld :: World -> GameChannelMessage -> World
reduceWorld world (RegisterClientMessage message) =
  (addPlayer world (C.player message)) {clients = clientHandle message : clients world}
reduceWorld world (UnregisterClientMessage clientIdentifier) =
  let newClients = filter (unregisterClientPredicate_ clientIdentifier) $ clients world
   in (removePlayerByUsername world clientIdentifier) {clients = newClients}
reduceWorld world (PlayerWalkMessage playerName startPos isRunning steps) = updatePlayer world playerName (P.issueWalkCommand (startPos, isRunning, steps))
reduceWorld world (InterfaceButtonClickMessage playerName button) =
  dispatchInterfaceButtonClick world playerName button
reduceWorld world (PlayerChatMessage playerName message) =
  updatePlayer world playerName (\p -> P.queueUpdate p (SayChatMessage message))
reduceWorld world UpdateWorldMessage =
  advance world

unregisterClientPredicate_ :: String -> ClientHandle -> Bool
unregisterClientPredicate_ removed clientHandle =
  removed /= username clientHandle
