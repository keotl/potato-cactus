{-# LANGUAGE OverloadedStrings #-}

module PotatoCactus.Network.SocketHandler where

import Control.Concurrent (forkIO)
import Control.Monad (forever, unless, void)
import qualified Data.Binary as B
import Data.Binary.Strict.Get
import qualified Data.ByteString as S
import Data.ByteString.Builder (byteStringHex)
import Data.ByteString.UTF8 as BSU
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import PotatoCactus.Login.LoginHandler (handleLogin)
import Data.Either

socketMain :: Socket -> IO ()
socketMain sock = do
  msg <- recv sock 1
  -- let decoded =
  case runGet getWord8 msg of
        (Right val, _) -> dispatch val sock
        (Left val, _) -> return ()
        
dispatch :: B.Word8 -> Socket -> IO ()
dispatch 14 = handleLogin -- actual login
dispatch 48 = handleLogin -- '0'
dispatch op = do
  print -- todo how to actually print op??


socketMain2 :: Socket -> IO ()
socketMain2 sock = do
  msg <- recv sock 1024
  unless (S.null msg) $ do
    sendAll sock msg
    socketMain sock
