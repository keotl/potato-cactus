module PotatoCactus.Game.Reducer where

import PotatoCactus.Boot.GameChannel (GameChannelMessage (InterfaceButtonClickMessage, PlayerChatMessage, PlayerWalkMessage, RegisterClientMessage, UnregisterClientMessage, UpdateWorldMessage), RegisterClientPayload (clientHandle))
import qualified PotatoCactus.Boot.GameChannel as C
import PotatoCactus.Game.Interface.InterfaceButtonDispatch (dispatchInterfaceButtonClick)
import qualified PotatoCactus.Game.Player as P
import PotatoCactus.Game.PlayerUpdate.PlayerUpdate (PlayerUpdate (SayChatMessage))
import PotatoCactus.Game.Typing (advance)
import PotatoCactus.Game.World (ClientHandle (username), World (World, clients, players, tick), updatePlayer)

reduceWorld :: World -> GameChannelMessage -> World
reduceWorld world (RegisterClientMessage message) =
  World (tick world) (C.player message : players world) (clientHandle message : clients world)
reduceWorld world (UnregisterClientMessage clientIdentifier) =
  let newClients = filter (unregisterClientPredicate_ clientIdentifier) $ clients world
   in World (tick world) (unregisterPlayer_ clientIdentifier $ players world) newClients
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

unregisterPlayer_ :: String -> [P.Player] -> [P.Player]
unregisterPlayer_ name =
  filter (\x -> P.username x /= name)
