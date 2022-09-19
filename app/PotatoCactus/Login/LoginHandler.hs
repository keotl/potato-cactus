{-# LANGUAGE OverloadedStrings #-}
module PotatoCactus.Login.LoginHandler where

import PotatoCactus.Game.Player as P
import PotatoCactus.Login.Models as L
import Network.Socket (Socket)
import Network.Socket.ByteString (sendAll)
import Data.ByteString.UTF8 as BSU

login :: LoginRequest -> Maybe Player
login r =
  Just Player {P.username = L.username r}

handleLogin :: Socket -> IO()
handleLogin sock = do
  sendAll sock $ BSU.fromString "ok"
