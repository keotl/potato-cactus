module PotatoCactus.Network.ClientHandler where

import Control.Concurrent (Chan, forkFinally, newChan, readChan, threadDelay, writeChan)
import Data.Binary.BitPut (runBitPut)
import Data.ByteString (pack)
import Data.ByteString.Builder (toLazyByteString, word16BE, word8)
import Data.ByteString.Builder.Extra (runBuilder)
import Data.ByteString.Lazy (toStrict)
import Data.IORef (readIORef)
import Network.Socket (Socket)
import Network.Socket.ByteString (recv, sendAll)
import PotatoCactus.Boot.GameChannel (gameChannel)
import PotatoCactus.Client.ClientUpdate (updateClient)
import PotatoCactus.Client.PlayerInit (playerInit)
import PotatoCactus.Game.World (ClientHandle (controlChannel, username), ClientHandleMessage, worldInstance)
import PotatoCactus.Network.InboundPacketMapper (mapPacket)
import PotatoCactus.Network.Packets.Opcodes
import PotatoCactus.Network.Packets.Out.InitializePlayerPacket (initializePlayerPacket)
import PotatoCactus.Network.Packets.Reader (InboundPacket (opcode), readPacket)

type InternalQueueMessage_ = Either ClientHandleMessage InboundPacket

clientHandlerMain :: ClientHandle -> Socket -> IO ()
clientHandlerMain handle sock = do
  putStrLn "started client handler main"
  internalQueue <- newChan
  controlChannelPollerThreadId <- forkFinally (controlChannelPoller_ (controlChannel handle) internalQueue) (\x -> print "control Channel poller exited")
  socketPollerThreadId <- forkFinally (socketPoller_ sock internalQueue) (\x -> print "socket poller exited")
  sendAll sock $ toStrict $ runBitPut $ playerInit handle
  clientHandlerMainLoop_ handle sock internalQueue

clientHandlerMainLoop_ :: ClientHandle -> Socket -> Chan InternalQueueMessage_ -> IO ()
clientHandlerMainLoop_ client sock chan = do
  message <- readChan chan
  case message of
    Left clientHandleMessage -> do
      updateClient sock client clientHandleMessage
      clientHandlerMainLoop_ client sock chan
    Right clientPacket -> do
      case mapPacket (username client) clientPacket of
        Just downstreamMessage -> do
          writeChan gameChannel downstreamMessage
          if opcode clientPacket == socketClosedOpcode
            then return () -- TODO - disconnect player  - keotl 2023-02-07
            else clientHandlerMainLoop_ client sock chan
        _ -> clientHandlerMainLoop_ client sock chan

  putStrLn "got out!"
  return ()

-- world <- readIORef worldInstance

-- hardcoded load map packet
-- let x = toStrict $ toLazyByteString $ mconcat [word8 73, word16BE 3093, word16BE 3244]
-- sendAll sock x
-- print "sent all to sock"
-- threadDelay $ 1000000 * 10

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
