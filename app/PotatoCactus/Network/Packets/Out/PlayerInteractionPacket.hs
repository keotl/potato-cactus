module PotatoCactus.Network.Packets.Out.PlayerInteractionPacket where

import Data.Binary.BitPut (putByteString, putNBits, runBitPut)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import qualified PotatoCactus.Game.Interface.PlayerInteraction as I (PlayerInteractionDefinition (index, name, pinned))
import PotatoCactus.Network.Binary (encodeStr, toWord_)

showPlayerInteractionPacket :: I.PlayerInteractionDefinition -> ByteString
showPlayerInteractionPacket interaction =
  toStrict $
    runBitPut
      ( do
          putNBits 8 $ toWord_ 104
          putNBits 8 $ toWord_ (- I.index interaction)
          putNBits 8 $ 128 + pinned_ interaction
          putByteString $ encodeStr (I.name interaction)
      )

pinned_ :: I.PlayerInteractionDefinition -> Int
pinned_ interaction =
  if I.pinned interaction then 1 else 0
