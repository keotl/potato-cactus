module PotatoCactus.Game.Reducer where

import PotatoCactus.Boot.GameChannel (GameChannelMessage (RegisterClientMessage, UnregisterClientMessage, UpdateWorldMessage), RegisterClientPayload (clientHandle))
import qualified PotatoCactus.Boot.GameChannel as C
import qualified PotatoCactus.Game.Player as P
import PotatoCactus.Game.World (ClientHandle (username), World (World, clients, players, tick))

reduceWorld :: World -> GameChannelMessage -> World
reduceWorld world (RegisterClientMessage message) =
  World (tick world) (C.player message : players world) (clientHandle message : clients world)
reduceWorld world (UnregisterClientMessage clientIdentifier) =
  let newClients = filter (unregisterClientPredicate_ clientIdentifier) $ clients world
   in World (tick world) (unregisterPlayer_ clientIdentifier $ players world) newClients
reduceWorld world UpdateWorldMessage =
  World (tick world + 1) (players world) (clients world)
reduceWorld state _ = state

unregisterClientPredicate_ :: String -> ClientHandle -> Bool
unregisterClientPredicate_ removed clientHandle =
  removed /= username clientHandle

unregisterPlayer_ :: String -> [P.Player] -> [P.Player]
unregisterPlayer_ name =
  filter (\x -> P.username x /= name)
