module PotatoCactus.Network.Packets.Packet where

import Data.Binary (Word8)
import Data.Binary.BitPut (BitPut, putByteString, putNBits, runBitPut)
import Data.ByteString (ByteString, length)
import Data.ByteString.Lazy (toStrict)
import PotatoCactus.Network.Binary (toShort_, toShortLE_)

fixedPacket :: Word8 -> BitPut -> ByteString
fixedPacket opcode payload =
  toStrict $
    runBitPut
      ( do
          putNBits 8 opcode
          payload
      )

-- Variable size packet, size encoded as ubyte
varPacket :: Word8 -> BitPut -> ByteString
varPacket opcode payload =
  let encodedPayload = toStrict $ runBitPut payload
   in toStrict $
        runBitPut
          ( do
              putNBits 8 opcode
              putNBits 8 $ Data.ByteString.length encodedPayload
              payload
          )

-- Variable size packet, size encoded as uint16
varShortPacket :: Word8 -> BitPut -> ByteString
varShortPacket opcode payload =
  let encodedPayload = toStrict $ runBitPut payload
   in toStrict $
        runBitPut
          ( do
              putNBits 8 opcode
              putNBits 16 $ toShort_ $ Data.ByteString.length encodedPayload
              payload
          )
