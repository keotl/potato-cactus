{-# LANGUAGE OverloadedStrings #-}

module PotatoCactus.Network.SocketHandler where

import Control.Concurrent (forkIO, newChan, writeChan)
import Control.Monad (forever, unless, void)
import qualified Data.Binary as B
import Data.Binary.Strict.Get
import qualified Data.ByteString as S
import Data.ByteString.Builder (byteStringHex)
import Data.ByteString.UTF8 as BSU
import Data.Either
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import PotatoCactus.Game.Player (Player (username))
import PotatoCactus.Game.World (ClientHandle (ClientHandle))
import PotatoCactus.Login.LoginHandler (handleLogin)
import PotatoCactus.Network.ClientHandler (clientHandlerMain)
import PotatoCactus.Game.Message.RegisterClientPayload (RegisterClientPayload(RegisterClientPayload, clientHandle, player))
import PotatoCactus.Boot.GameChannel (gameChannel)
import PotatoCactus.Game.Message.GameChannelMessage (GameChannelMessage(RegisterClientMessage))

socketMain :: Socket -> IO ()
socketMain sock = do
  msg <- recv sock 1
  case runGet getWord8 msg of
    (Right val, _) -> dispatch val sock
    (Left val, _) -> return ()

  case msg of
    "" -> return ()
    _ -> socketMain sock

dispatch :: B.Word8 -> Socket -> IO ()
dispatch 14 =
  \sock -> do
    player <- handleLogin sock
    case player of
      Nothing -> return ()
      (Just p) -> do
        chan <- newChan
        let client = ClientHandle (username p) chan
        writeChan gameChannel $ RegisterClientMessage (RegisterClientPayload {clientHandle = client, player = p})
        clientHandlerMain client p sock

dispatch op = \x -> do
  putStrLn $ "Unknown opcode: " ++ show op
  print x

socketMain2 :: Socket -> IO ()
socketMain2 sock = do
  msg <- recv sock 1024
  unless (S.null msg) $ do
    sendAll sock msg
    socketMain sock
