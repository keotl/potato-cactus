module PotatoCactus.Game.Reducer where

import PotatoCactus.Boot.GameChannel (GameChannelMessage (RegisterClientMessage, UnregisterClientMessage), RegisterClientPayload (clientHandle))
import PotatoCactus.Game.World (ClientHandle (username), World (World, clients, players, tick))

reduceWorld :: World -> GameChannelMessage -> World
reduceWorld world (RegisterClientMessage message) =
  World (tick world) (players world) (clientHandle message : clients world)
reduceWorld world (UnregisterClientMessage clientIdentifier) =
  let newClients = filter (unregisterClientPredicate_ clientIdentifier) $ clients world
   in World (tick world) (players world) newClients
reduceWorld state _ = state

unregisterClientPredicate_ :: String -> ClientHandle -> Bool
unregisterClientPredicate_ removed clientHandle =
  removed /= username clientHandle
