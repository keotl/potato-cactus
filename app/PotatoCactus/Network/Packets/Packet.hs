module PotatoCactus.Network.Packets.Packet where

import Data.Binary (Put, Word8, putWord8)
import Data.Binary.BitPut (BitPut, putByteString, putNBits, runBitPut)
import Data.Binary.Put (putWord16be, runPut)
import Data.ByteString (ByteString, length)
import Data.ByteString.Lazy (toStrict)
import PotatoCactus.Network.Binary (toShortLE_, toShort_)

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

varShortPacket2 :: Word8 -> Put -> ByteString
varShortPacket2 opcode payload =
  let encodedPayload = toStrict $ runPut payload
   in toStrict $
        runPut
          ( do
              putWord8 opcode
              putWord16be $ fromIntegral $ Data.ByteString.length encodedPayload
              payload
          )
