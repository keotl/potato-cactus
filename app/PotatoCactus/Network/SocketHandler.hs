module PotatoCactus.Network.SocketHandler where

import Control.Concurrent (forkIO)
import Network.Socket
import qualified Data.ByteString as S
import Network.Socket.ByteString (recv, sendAll)
import Control.Monad (unless, forever, void)

socketMain :: Socket -> IO()
socketMain sock = do
  msg <- recv sock 1024
  unless (S.null msg) $ do
    sendAll sock msg
    socketMain sock
