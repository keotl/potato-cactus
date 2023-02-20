module PotatoCactus.Network.Packets.Out.PlayerUpdate.EncodeChatUpdate (encodeChatUpdateBlock) where

import Data.Binary.BitPut (BitPut, putByteString, putNBits, runBitPut)
import Data.Bits (Bits (xor), (.&.))
import Data.ByteString (ByteString, length, pack, reverse, unpack)
import Data.ByteString.Lazy (toStrict)
import PotatoCactus.Game.Player (Player (updateMask), chatMessage)
import PotatoCactus.Game.PlayerUpdate.ChatMessage (ChatMessage (color, effect, message))
import PotatoCactus.Game.PlayerUpdate.UpdateMask (chatFlag)
import PotatoCactus.Game.World (World (World))
import PotatoCactus.Network.Binary (toShortLE_, toWord_)
import PotatoCactus.Network.Encoding.ChatMessageEncoding (encodeChatText)
import PotatoCactus.Network.Packets.Out.PlayerUpdate.EncodeBlock (addBlockIfRequired)

-- encodeChatUpdateBlock :: Player -> World -> ByteString
-- encodeChatUpdateBlock = addBlockIfRequired chatFlag encode_

encodeChatUpdateBlock :: Player -> World -> ByteString
encodeChatUpdateBlock p w =
  toStrict $
    runBitPut
      ( case chatMessage p of
          Just m -> do
            putNBits 8 $ toWord_ (effect m)
            putNBits 8 $ toWord_ (color m)
            putNBits 8 $ toWord_ 0 -- player rights
            let messageBytes = messageBytes_ (encodeChatText (message m))
             in do
                  putNBits 8 $ toWord_ (- Data.ByteString.length messageBytes)
                  putByteString messageBytes
          Nothing -> putNBits 0 $ toWord_ 0
      )

messageBytes_ :: ByteString -> ByteString
messageBytes_ x =
  Data.ByteString.reverse $
    Data.ByteString.pack $
      map
        (`xor` 128)
        (Data.ByteString.unpack x)
