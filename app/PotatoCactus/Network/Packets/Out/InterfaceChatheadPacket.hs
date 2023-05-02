module PotatoCactus.Network.Packets.Out.InterfaceChatheadPacket where

import Data.Binary.Put (putWord16le)
import Data.Bits (xor)
import Data.ByteString (ByteString)
import Data.Word (Word16)
import PotatoCactus.Game.Definitions.NpcDefinitions (NpcDefinitionId)
import PotatoCactus.Network.Packets.Packet (fixedPacket2)

interfaceNpcChatheadPacket :: Word16 -> NpcDefinitionId -> ByteString
interfaceNpcChatheadPacket interfaceId npcId =
  fixedPacket2
    75
    ( do
        putWord16le . fromIntegral $ (npcId `xor` 128)
        putWord16le (interfaceId `xor` 128)
    )

interfacePlayerChatheadPacket :: Word16 -> ByteString
interfacePlayerChatheadPacket interfaceId =
  fixedPacket2
    185
    ( do
        putWord16le (interfaceId `xor` 128)
    )
