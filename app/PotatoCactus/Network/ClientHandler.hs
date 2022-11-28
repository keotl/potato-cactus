module PotatoCactus.Network.ClientHandler where

import Control.Concurrent (Chan, forkFinally, newChan, readChan, threadDelay, writeChan)
import Data.ByteString (pack)
import Data.ByteString.Builder (toLazyByteString, word16BE, word8)
import Data.ByteString.Builder.Extra (runBuilder)
import Data.ByteString.Lazy (toStrict)
import Data.IORef (readIORef)
import Network.Socket (Socket)
import Network.Socket.ByteString (recv, sendAll)
import PotatoCactus.Game.World (worldInstance, ClientHandleMessage, ClientHandle (controlChannel))
import PotatoCactus.Network.Packets.Reader (InboundPacket, readPacket)


type InternalQueueMessage_ = Either ClientHandleMessage InboundPacket

clientHandlerMain :: ClientHandle -> Socket -> IO ()
clientHandlerMain handle sock = do
  putStrLn "started client handler main"
  internalQueue <- newChan
  controlChannelPollerThreadId <- forkFinally (controlChannelPoller_ (controlChannel handle) internalQueue) (\x -> print "control Channel poller exited")
  socketPollerThreadId <- forkFinally (socketPoller_ sock internalQueue) (\x -> print "socket poller exited")

  -- TODO poll both socket and control channel without blocking??
  clientHandlerMainLoop_ sock internalQueue

clientHandlerMainLoop_ :: Socket -> Chan InternalQueueMessage_ -> IO ()
clientHandlerMainLoop_ sock chan = do
  -- todo read chan
  world <- readIORef worldInstance

  -- hardcoded load map packet
  let x = toStrict $ toLazyByteString $ mconcat [word8 73, word16BE 3093, word16BE 3244]
  sendAll sock x

  threadDelay $ 1000000 * 10

  clientHandlerMainLoop_ sock chan

controlChannelPoller_ :: Chan ClientHandleMessage -> Chan InternalQueueMessage_ -> IO ()
controlChannelPoller_ input output = do
  message <- readChan input
  writeChan output $ Left message
  controlChannelPoller_ input output

socketPoller_ :: Socket -> Chan InternalQueueMessage_ -> IO ()
socketPoller_ sock output = do
  packet <- readPacket sock
  writeChan output $ Right packet
  socketPoller_ sock output
