{-# LANGUAGE OverloadedStrings #-}

module PotatoCactus.Network.SocketHandler where

import Control.Concurrent (forkIO, writeChan, newChan)
import Control.Monad (forever, unless, void)
import qualified Data.Binary as B
import Data.Binary.Strict.Get
import qualified Data.ByteString as S
import Data.ByteString.Builder (byteStringHex)
import Data.ByteString.UTF8 as BSU
import Data.Either
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import PotatoCactus.Login.LoginHandler (handleLogin)
import PotatoCactus.Boot.GameChannel (gameChannel, RegisterClientMessage (RegisterClientMessage))
import PotatoCactus.Network.ClientHandle (ClientHandle(ClientHandle))
import PotatoCactus.Game.Player (Player(username))

socketMain :: Socket -> IO ()
socketMain sock = do
  msg <- recv sock 1
  case runGet getWord8 msg of
    (Right val, _) -> dispatch val sock
    (Left val, _) -> return ()
  socketMain sock

dispatch :: B.Word8 -> Socket -> IO ()
dispatch 14 =
  \sock -> do
  player <- handleLogin sock
  case player of
    Nothing -> return ()
    (Just p) -> do
      chan <- newChan
      writeChan gameChannel $ RegisterClientMessage (ClientHandle (username p) chan)

  -- return ()

-- dispatch 48 = handleLogin -- '0' for easier testing
dispatch op = \x -> do
  putStrLn $ "Unknown opcode: " ++ show op
  print x

socketMain2 :: Socket -> IO ()
socketMain2 sock = do
  msg <- recv sock 1024
  unless (S.null msg) $ do
    sendAll sock msg
    socketMain sock
