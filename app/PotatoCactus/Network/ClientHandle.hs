module PotatoCactus.Network.ClientHandle where

import Control.Concurrent (Chan, forkFinally, newChan, readChan, writeChan)
import Network.Socket (Socket)
import Network.Socket.ByteString (recv)
import PotatoCactus.Network.Packets.Reader (InboundPacket, readPacket)

type ClientHandleMessage = String -- TODO create types

data ClientHandle = ClientHandle
  { username :: String,
    controlChannel :: Chan ClientHandleMessage
  }

type InternalQueueMessage_ = Either ClientHandleMessage InboundPacket

clientHandlerMain :: ClientHandle -> Socket -> IO ()
clientHandlerMain handle sock = do
  putStrLn "started client handler main"
  internalQueue <- newChan
  controlChannelPollerThreadId <- forkFinally (controlChannelPoller_ (controlChannel handle) internalQueue) (\x -> print "control Channel poller exited")
  socketPollerThreadId <- forkFinally (socketPoller_ sock internalQueue) (\x -> print "socket poller exited")
  
  -- TODO poll both socket and control channel without blocking??
  return ()

clientHandlerMainLoop_ :: Chan InternalQueueMessage_ -> IO ()
clientHandlerMainLoop_ chan = do
  
  clientHandlerMainLoop_ chan

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
