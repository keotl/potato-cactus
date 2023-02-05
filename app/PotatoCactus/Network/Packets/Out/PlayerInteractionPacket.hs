module PotatoCactus.Network.Packets.Out.PlayerInteractionPacket where

import Data.Binary.BitPut (putByteString, putNBits, runBitPut)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import qualified PotatoCactus.Game.Interface.PlayerInteraction as I (PlayerInteractionDefinition (index, name, pinned))
import PotatoCactus.Network.Binary (encodeStr, toWord_)
import PotatoCactus.Network.Packets.Packet (varPacket)

showPlayerInteractionPacket :: I.PlayerInteractionDefinition -> ByteString
showPlayerInteractionPacket interaction =
  varPacket
    104
    ( do
        putNBits 8 $ toWord_ (- I.index interaction) -- broken
        putNBits 8 $ 128 + pinned_ interaction
        putByteString $ encodeStr (I.name interaction)
    )

pinned_ :: I.PlayerInteractionDefinition -> Int
pinned_ interaction =
  if I.pinned interaction then 1 else 0
