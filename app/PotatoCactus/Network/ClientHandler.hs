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
import PotatoCactus.Client.ClientUpdate (ClientLocalState_, defaultState, updateClient)
import PotatoCactus.Client.PlayerInit (playerInit)
import PotatoCactus.Game.Player (Player)
import PotatoCactus.Game.World (ClientHandle (controlChannel, username), ClientHandleMessage (CloseClientConnectionMessage), worldInstance)
import PotatoCactus.Network.InboundPacketMapper (mapPacket)
import PotatoCactus.Network.Packets.Opcodes
import PotatoCactus.Network.Packets.Out.InitializePlayerPacket (initializePlayerPacket)
import PotatoCactus.Network.Packets.Reader (InboundPacket (opcode), readPacket)
import PotatoCactus.Utils.Logging (LogLevel (Debug, Info), logger)
import PotatoCactus.Boot.GameChannel (gameChannel)
import PotatoCactus.Game.Message.GameChannelMessage (GameChannelMessage(UnregisterClientMessage))

type InternalQueueMessage_ = Either ClientHandleMessage InboundPacket

clientHandlerMain :: ClientHandle -> Player -> Socket -> IO ()
clientHandlerMain handle player sock = do
  logger_ Debug $ "Started client handler for " ++ username handle ++ "."
  internalQueue <- newChan
  controlChannelPollerThreadId <-
    forkFinally
      (controlChannelPoller_ (controlChannel handle) internalQueue)
      ( \x ->
          logger_ Debug "control Channel poller exited."
      )
  socketPollerThreadId <-
    forkFinally
      (socketPoller_ sock internalQueue)
      ( \x -> writeChan gameChannel $ UnregisterClientMessage (username handle)
      )
  sendAll sock $ toStrict $ runBitPut $ playerInit handle player
  clientHandlerMainLoop_ handle defaultState sock internalQueue

clientHandlerMainLoop_ :: ClientHandle -> ClientLocalState_ -> Socket -> Chan InternalQueueMessage_ -> IO ()
clientHandlerMainLoop_ client clientState sock chan = do
  message <- readChan chan
  case message of
    Left clientHandleMessage -> do
      updatedState <- updateClient sock client clientState clientHandleMessage
      case clientHandleMessage of
        CloseClientConnectionMessage -> logger_ Info $ username client ++ " disconnected."
        _ -> clientHandlerMainLoop_ client updatedState sock chan
    Right clientPacket -> do
      case mapPacket (username client) clientPacket of
        Just downstreamMessage -> do
          writeChan gameChannel downstreamMessage
          if opcode clientPacket == socketClosedOpcode
            then logger_ Info $ username client ++ " disconnected."
            else clientHandlerMainLoop_ client clientState sock chan
        _ -> clientHandlerMainLoop_ client clientState sock chan

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

logger_ = logger "ClientHandler"
