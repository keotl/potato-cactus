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
import PotatoCactus.Game.Player (Player (Player, username))
import qualified PotatoCactus.Game.World as W (ClientHandle, ClientHandleMessage (WorldUpdatedMessage), World (players), username, worldInstance)
import Type.Reflection (typeOf)

updateClient :: Socket -> W.ClientHandle -> W.ClientHandleMessage -> IO ()
updateClient sock client W.WorldUpdatedMessage = do
  world <- readIORef W.worldInstance
  putStrLn $ "world updated for client " ++ W.username client
  -- TODO region update
  -- Local Player Update (Opcode 81)
  -- putStrLn $ "current world players" ++
  sendAll sock (playerUpdate mockPlayer_ world)
  -- TODO - figure out why player is not found  - keotl 2023-02-06
  -- print $ (map (username) (W.players world))
  -- let player = find (\x -> (username x) == (W.username client)) (W.players world)
  -- case player of
  --   Just p -> do
  --     putStrLn $ "found player " ++ (username p)
  --     sendAll sock (playerUpdate mockPlayer_ world)
  --   Nothing -> putStrLn "could not find player"
  -- NPC Update
  return ()

mockPlayer_ :: Player
mockPlayer_ =
  Player
    { username = "auie"
    }
