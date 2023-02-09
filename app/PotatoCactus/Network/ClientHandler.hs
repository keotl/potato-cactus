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
import PotatoCactus.Boot.GameChannel (GameChannelMessage (UnregisterClientMessage), gameChannel)
import PotatoCactus.Client.ClientUpdate (updateClient)
import PotatoCactus.Client.PlayerInit (playerInit)
import PotatoCactus.Game.Player (Player)
import PotatoCactus.Game.World (ClientHandle (controlChannel, username), ClientHandleMessage (CloseClientConnectionMessage), worldInstance)
import PotatoCactus.Network.InboundPacketMapper (mapPacket)
import PotatoCactus.Network.Packets.Opcodes
import PotatoCactus.Network.Packets.Out.InitializePlayerPacket (initializePlayerPacket)
import PotatoCactus.Network.Packets.Reader (InboundPacket (opcode), readPacket)

type InternalQueueMessage_ = Either ClientHandleMessage InboundPacket

clientHandlerMain :: ClientHandle -> Player -> Socket -> IO ()
clientHandlerMain handle player sock = do
  putStrLn "started client handler main"
  internalQueue <- newChan
  controlChannelPollerThreadId <- forkFinally (controlChannelPoller_ (controlChannel handle) internalQueue) (\x -> print "control Channel poller exited")
  socketPollerThreadId <-
    forkFinally
      (socketPoller_ sock internalQueue)
      ( \x -> writeChan gameChannel $ UnregisterClientMessage (username handle)
      )
  sendAll sock $ toStrict $ runBitPut $ playerInit handle player
  clientHandlerMainLoop_ handle sock internalQueue

clientHandlerMainLoop_ :: ClientHandle -> Socket -> Chan InternalQueueMessage_ -> IO ()
clientHandlerMainLoop_ client sock chan = do
  message <- readChan chan
  case message of
    Left clientHandleMessage -> do
      updateClient sock client clientHandleMessage
      case clientHandleMessage of
        CloseClientConnectionMessage -> return ()
        _ -> clientHandlerMainLoop_ client sock chan
    Right clientPacket -> do
      case mapPacket (username client) clientPacket of
        Just downstreamMessage -> do
          writeChan gameChannel downstreamMessage
          if opcode clientPacket == socketClosedOpcode
            then return ()
            else clientHandlerMainLoop_ client sock chan
        _ -> clientHandlerMainLoop_ client sock chan

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
