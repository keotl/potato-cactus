{-# LANGUAGE OverloadedStrings #-}

module PotatoCactus.Client.ClientUpdate (updateClient, defaultState, ClientLocalState_ (localPlayerIndex)) where

import Data.Binary.BitPut (runBitPut)
import Data.ByteString (pack)
import Data.ByteString.Lazy (toStrict)
import Data.List
import GHC.IORef (readIORef)
import Network.Socket
import Network.Socket.ByteString (recv, send, sendAll)
import PotatoCactus.Client.GameObjectUpdate.EncodeGameObjectUpdate (encodeGameObjectUpdate)
import PotatoCactus.Client.LocalEntityList (LocalEntityList, updateLocalEntities)
import PotatoCactus.Game.Entity.Npc.Npc (Npc)
import PotatoCactus.Game.Entity.Object.DynamicObjectCollection (DynamicObject)
import PotatoCactus.Game.Entity.Object.GameObject (GameObject (GameObject))
import PotatoCactus.Game.Message.ObjectClickPayload (ObjectClickPayload (ObjectClickPayload))
import PotatoCactus.Game.Movement.MovementEntity (MovementEntity (PlayerWalkMovement_), hasChangedRegion)
import PotatoCactus.Game.Movement.PlayerWalkMovement (PlayerWalkMovement (lastRegionUpdate_))
import PotatoCactus.Game.Movement.PositionXY (fromXY, toXY)
import PotatoCactus.Game.Player (Player (Player, equipment, inventory, movement, serverIndex, username))
import PotatoCactus.Game.PlayerUpdate.Equipment (Equipment (container))
import PotatoCactus.Game.Position (GetPosition (getPosition), Position (Position, x, y))
import qualified PotatoCactus.Game.World as W (ClientHandle, ClientHandleMessage (CloseClientConnectionMessage, WorldUpdatedMessage), World (players, tick), username, worldInstance)
import PotatoCactus.Game.World.MobList (findByIndex, findByPredicate)
import qualified PotatoCactus.Game.World.Selectors as WS
import PotatoCactus.Network.Packets.Out.AddObjectPacket (addObjectPacket)
import PotatoCactus.Network.Packets.Out.ClearChunkObjectsPacket (clearChunkObjectsPacket, clearChunksAroundPlayer)
import PotatoCactus.Network.Packets.Out.LoadMapRegionPacket (loadMapRegionPacket)
import PotatoCactus.Network.Packets.Out.NpcUpdate.NpcUpdatePacket (npcUpdatePacket)
import PotatoCactus.Network.Packets.Out.PlayerUpdate.PlayerUpdatePacket (playerUpdatePacket)
import PotatoCactus.Network.Packets.Out.RemoveObjectPacket (removeObjectPacket)
import PotatoCactus.Network.Packets.Out.SetPlacementReferencePacket (setPlacementReferencePacket)
import PotatoCactus.Network.Packets.Out.UpdateItemContainerPacket (updateItemContainerPacket)
import PotatoCactus.Network.Packets.Out.UpdateRunEnergyPacket (updateRunEnergyPacket)
import Type.Reflection (typeOf)

data ClientLocalState_ = ClientLocalState_
  { localPlayers :: LocalEntityList Player,
    localNpcs :: LocalEntityList Npc,
    gameObjects :: [DynamicObject],
    localPlayerIndex :: Int
  }

defaultState =
  ClientLocalState_
    { localPlayers = [],
      localNpcs = [],
      gameObjects = [],
      localPlayerIndex = -1
    }

updateClient :: Socket -> W.ClientHandle -> ClientLocalState_ -> W.ClientHandleMessage -> IO ClientLocalState_
updateClient sock client localState W.WorldUpdatedMessage = do
  world <- readIORef W.worldInstance
  case findPlayer_ world (localPlayerIndex localState) (W.username client) of
    Just p -> do
      if hasChangedRegion (movement p)
        then do
          sendAll sock $ loadMapRegionPacket (getPosition p)
        else pure ()

      let newLocalPlayers =
            updateLocalEntities
              (localPlayers localState)
              (WS.localPlayers world p)
       in let newLocalNpcs = updateLocalEntities (localNpcs localState) (WS.localNpcs world p)
           in do
                sendAll sock (playerUpdatePacket p newLocalPlayers world)
                sendAll sock (npcUpdatePacket p newLocalNpcs world)

                sendAll sock (updateItemContainerPacket (inventory p))
                sendAll sock (updateItemContainerPacket (container (equipment p)))

                sendAll sock (updateRunEnergyPacket 100)

                -- case clickedEntity world of
                --   Nothing -> pure ()
                --   Just (ObjectClickPayload objectId position index) -> do
                --     -- to add an object :
                --     -- 1. set a point of reference in relation to the player's last update chunk base
                --     -- 2. Add the object in relation to that point of reference.
                --     -- The point of reference has to be selected so that the offset is positive to the object
                --     -- Probably easiest to set the reference each time an object is sent

                --     -- sendAll sock (removeObjectPacket (getPosition p) (GameObject objectId (fromXY position 0)))
                --     -- putStrLn $ "sending addobject" ++ (show ((getPosition p) {x = 1 + x (getPosition p)}))
                --     sendAll sock $ setPlacementReferencePacket p (fromXY position 0)
                --     if (objectId == 1531)
                --       then sendAll sock (removeObjectPacket (fromXY position 0) (GameObject objectId (fromXY position 0) 0))
                --       else
                --         sendAll
                --           sock
                --           ( addObjectPacket
                --               (fromXY position 0)
                --               -- ((getPosition p) {x = 1 + x (getPosition p)})
                --               ( GameObject
                --                   (objectId + 1)
                --                   (fromXY position 0)
                --                   0
                --                   -- ((getPosition p) {x = 1 + x (getPosition p)})
                --               )
                --           )
                let (newObjects, packets) = encodeGameObjectUpdate (gameObjects localState) world p
                 in do
                      sendAll sock packets
                      return
                        ClientLocalState_
                          { localPlayers = newLocalPlayers,
                            localNpcs = newLocalNpcs,
                            localPlayerIndex = serverIndex p,
                            gameObjects = newObjects
                          }
    Nothing -> do
      putStrLn $ "could not find player " ++ W.username client
      return localState
updateClient _ _ _ W.CloseClientConnectionMessage = return defaultState

findPlayer_ :: W.World -> Int -> String -> Maybe Player
findPlayer_ world index playerName =
  if index /= -1
    then findByIndex (W.players world) index
    else findByPredicate (W.players world) (\p -> username p == playerName)
