module PotatoCactus.Network.Packets.Packet where

import Data.Binary (Word8)
import Data.Binary.BitPut (BitPut, putByteString, putNBits, runBitPut)
import Data.ByteString (ByteString, length)
import Data.ByteString.Lazy (toStrict)

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

fixedPacket :: Word8 -> BitPut -> ByteString
fixedPacket opcode payload =
  toStrict $
    runBitPut
      ( do
          putNBits 8 opcode
          payload
      )
