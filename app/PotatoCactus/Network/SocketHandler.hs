module PotatoCactus.Network.SocketHandler where

import Network.Socket
import System.IO (hSetBuffering, hGetLine, hPutStrLn, BufferMode(..), Handle)
import Control.Concurrent (forkIO)

-- https://catonmat.net/simple-haskell-tcp-server
