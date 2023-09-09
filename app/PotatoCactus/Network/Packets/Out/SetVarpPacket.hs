module PotatoCactus.Network.Packets.Out.SetVarpPacket where

import Data.Binary.Put (putWord16le, putWord32be)
import Data.ByteString (ByteString, concat, empty)
import qualified PotatoCactus.Game.PlayerUpdate.VarpSet as Varp
import PotatoCactus.Network.Binary (toIntME_)
import PotatoCactus.Network.Packets.Packet (fixedPacket2)

setVarpPacket :: Int -> Int -> ByteString
setVarpPacket varpId value =
  fixedPacket2
    87
    ( do
        putWord16le . fromIntegral $ varpId
        putWord32be . toIntME_ $ value
    )

encodeVarps :: [Varp.Varp] -> ByteString
encodeVarps varps =
  Data.ByteString.concat
    ( map
        ( \v ->
            setVarpPacket
              (Varp.varpId v)
              (fromIntegral . Varp.value $ v)
        )
        varps
    )
