module PotatoCactus.Network.Packets.In.ChatMessagePacket (playerChatMessage) where

import Control.Monad (replicateM)
import Data.Binary (Get, getWord8)
import Data.Binary.Get (runGet)
import Data.Bits (xor)
import Data.ByteString (ByteString, drop, length, reverse)
import Data.ByteString.Lazy (fromStrict)
import Data.Word (Word8)
import PotatoCactus.Boot.GameChannel
import PotatoCactus.Game.PlayerUpdate.ChatMessage (ChatMessage (ChatMessage))
import PotatoCactus.Network.Binary (nibbles)
import PotatoCactus.Network.Encoding.ChatMessageEncoding (decodeChatText)
import PotatoCactus.Network.Packets.Reader (InboundPacket (payload))

playerChatMessage :: String -> InboundPacket -> GameChannelMessage
playerChatMessage username packet =
  let [effect, colour] =
        runGet
          readPayload_
          (fromStrict $ payload packet)
   in PlayerChatMessage
        username
        ( ChatMessage
            ( decodeChatText $
                nibbles $
                  readBytes_ $
                    Data.ByteString.reverse (Data.ByteString.drop 2 (payload packet))
            )
            (fromIntegral colour)
            (fromIntegral effect)
        )

readPayload_ :: Get [Word8]
readPayload_ = do
  effect <- getWord8
  colour <- getWord8
  return [effect, colour]

readBytes_ :: ByteString -> [Word8]
readBytes_ input =
  runGet
    ( replicateM
        (Data.ByteString.length input)
        ( do
            a <- getWord8
            return (a `xor` 128)
        )
    )
    (fromStrict input)
