module PotatoCactus.Network.Packets.Out.PlayerUpdate.EncodeChatUpdate where

import Data.Binary.BitPut (BitPut, putByteString, putNBits)
import Data.Bits ((.&.))
import PotatoCactus.Game.Player (Player (updateMask), chatMessage)
import PotatoCactus.Game.PlayerUpdate.ChatMessage (ChatMessage (color, effect, message))
import PotatoCactus.Game.PlayerUpdate.UpdateMask (chatFlag)
import PotatoCactus.Network.Binary (encodeStrNonTerminated, toShortLE_, toWord_)
import PotatoCactus.Network.Packets.Out.PlayerUpdate.EncodeBlock (encodeBlock)

encodeChatUpdateBlock :: Player -> BitPut
encodeChatUpdateBlock = encodeBlock chatFlag encode_

encode_ :: Player -> BitPut
encode_ p =
  case chatMessage p of
    Just m -> do
      putNBits 8 $ toWord_ (effect m)
      putNBits 8 $ toWord_ (color m)
      putNBits 8 $ toWord_ 0 -- player rights
      putNBits 8 $ toWord_ (- length (message m))
      putByteString $ encodeStrNonTerminated (message m)
    Nothing -> putNBits 0 $ toWord_ 0
