{-# LANGUAGE OverloadedStrings #-}

module PotatoCactus.Client.ClientUpdate where

import Data.Binary.BitPut (runBitPut)
import Data.ByteString (pack)
import Data.ByteString.Lazy (toStrict)
import GHC.IORef (readIORef)
import Network.Socket
import Network.Socket.ByteString (recv, send, sendAll)
import PotatoCactus.Client.PlayerUpdate (playerUpdate)
import PotatoCactus.Game.World (ClientHandle (username), ClientHandleMessage (WorldUpdatedMessage), worldInstance)
import Type.Reflection (typeOf)

updateClient :: Socket -> ClientHandle -> ClientHandleMessage -> IO ()
updateClient sock client WorldUpdatedMessage = do
  world <- readIORef worldInstance
  putStrLn $ "world updated for client " ++ username client
  -- TODO region update
  -- Local Player Update (Opcode 81)
  sendAll sock $ toStrict $ runBitPut $ playerUpdate client world
  -- NPC Update
  return ()
