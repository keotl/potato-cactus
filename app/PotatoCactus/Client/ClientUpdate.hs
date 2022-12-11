{-# LANGUAGE OverloadedStrings #-}

module PotatoCactus.Client.ClientUpdate where

import GHC.IORef (readIORef)
import Network.Socket
import PotatoCactus.Game.World (ClientHandle (username), ClientHandleMessage (WorldUpdatedMessage), worldInstance)
import Type.Reflection (typeOf)
import Network.Socket.ByteString (recv, sendAll, send)
import Data.ByteString (pack)

updateClient :: Socket -> ClientHandle -> ClientHandleMessage -> IO ()
updateClient sock client WorldUpdatedMessage = do
  world <- readIORef worldInstance
  putStrLn $ "world updated for client " ++ username client
  -- Local Player Update (Opcode 81)
  sendAll sock $ pack [81]

  -- NPC Update
  return ()
