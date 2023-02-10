{-# LANGUAGE OverloadedStrings #-}

module PotatoCactus.Client.ClientUpdate where

import Data.Binary.BitPut (runBitPut)
import Data.ByteString (pack)
import Data.ByteString.Lazy (toStrict)
import Data.List
import GHC.IORef (readIORef)
import Network.Socket
import Network.Socket.ByteString (recv, send, sendAll)
import PotatoCactus.Client.PlayerUpdate (playerUpdate)
import PotatoCactus.Game.Movement.MovementEntity (hasChangedRegion)
import PotatoCactus.Game.Player (Player (Player, movement, username))
import PotatoCactus.Game.Position (GetPosition (getPosition))
import qualified PotatoCactus.Game.World as W (ClientHandle, ClientHandleMessage (CloseClientConnectionMessage, WorldUpdatedMessage), World (players, tick), username, worldInstance)
import PotatoCactus.Network.Packets.Out.LoadMapRegionPacket (loadMapRegionPacket)
import PotatoCactus.Network.Packets.Out.UpdateRunEnergyPacket (updateRunEnergyPacket)
import Type.Reflection (typeOf)

updateClient :: Socket -> W.ClientHandle -> W.ClientHandleMessage -> IO ()
updateClient sock client W.WorldUpdatedMessage = do
  world <- readIORef W.worldInstance

  let player = find (\x -> username x == W.username client) (W.players world)
  case player of
    Just p -> do
      if hasChangedRegion (movement p)
        then do
          sendAll sock (loadMapRegionPacket (getPosition p))
        else pure ()

      sendAll sock (playerUpdate p world)
      sendAll sock (updateRunEnergyPacket 66)
    -- TODO - NPC update - keotl 2023-02-08
    Nothing -> putStrLn $ "could not find player " ++ W.username client
updateClient _ _ W.CloseClientConnectionMessage = return ()
