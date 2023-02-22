{-# LANGUAGE OverloadedStrings #-}

module PotatoCactus.Client.ClientUpdate (updateClient, defaultState, ClientLocalState_) where

import Data.Binary.BitPut (runBitPut)
import Data.ByteString (pack)
import Data.ByteString.Lazy (toStrict)
import Data.List
import GHC.IORef (readIORef)
import Network.Socket
import Network.Socket.ByteString (recv, send, sendAll)
import PotatoCactus.Client.LocalPlayerList (LocalPlayerList, updateLocalPlayers)
import PotatoCactus.Game.Movement.MovementEntity (hasChangedRegion)
import PotatoCactus.Game.Player (Player (Player, equipment, inventory, movement, serverIndex, username))
import PotatoCactus.Game.PlayerUpdate.Equipment (Equipment (container))
import PotatoCactus.Game.Position (GetPosition (getPosition))
import qualified PotatoCactus.Game.World as W (ClientHandle, ClientHandleMessage (CloseClientConnectionMessage, WorldUpdatedMessage), World (players, tick), username, worldInstance)
import PotatoCactus.Game.World.MobList (findByIndex, findByPredicate)
import qualified PotatoCactus.Game.World.Selectors as WS
import PotatoCactus.Network.Packets.Out.LoadMapRegionPacket (loadMapRegionPacket)
import PotatoCactus.Network.Packets.Out.PlayerUpdate.PlayerUpdatePacket (playerUpdatePacket)
import PotatoCactus.Network.Packets.Out.UpdateItemContainerPacket (updateItemContainerPacket)
import PotatoCactus.Network.Packets.Out.UpdateRunEnergyPacket (updateRunEnergyPacket)
import Type.Reflection (typeOf)

data ClientLocalState_ = ClientLocalState_
  { localPlayers :: LocalPlayerList,
    localPlayerIndex :: Int
  }

defaultState = ClientLocalState_ {localPlayers = [], localPlayerIndex = -1}

updateClient :: Socket -> W.ClientHandle -> ClientLocalState_ -> W.ClientHandleMessage -> IO ClientLocalState_
updateClient sock client localState W.WorldUpdatedMessage = do
  world <- readIORef W.worldInstance
  case findPlayer_ world (localPlayerIndex localState) (W.username client) of
    Just p -> do
      if hasChangedRegion (movement p)
        then do
          sendAll sock (loadMapRegionPacket (getPosition p))
        else pure ()

      let newLocalPlayers =
            updateLocalPlayers
              (localPlayers localState)
              (WS.localPlayers world p)
       in do
            sendAll sock (playerUpdatePacket p newLocalPlayers world)
            sendAll sock (updateItemContainerPacket (inventory p))
            sendAll sock (updateItemContainerPacket (container (equipment p)))

            sendAll sock (updateRunEnergyPacket 100)

            return
              ClientLocalState_
                { localPlayers = newLocalPlayers,
                  localPlayerIndex = serverIndex p
                }
    -- TODO - NPC update - keotl 2023-02-08
    Nothing -> do
      putStrLn $ "could not find player " ++ W.username client
      return localState
updateClient _ _ _ W.CloseClientConnectionMessage = return defaultState

findPlayer_ :: W.World -> Int -> String -> Maybe Player
findPlayer_ world index playerName =
  if index /= -1
    then findByIndex (W.players world) index
    else findByPredicate (W.players world) (\p -> username p == playerName)
