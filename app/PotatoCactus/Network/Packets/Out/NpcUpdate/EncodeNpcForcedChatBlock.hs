module PotatoCactus.Network.Packets.Out.NpcUpdate.EncodeNpcForcedChatBlock where

import Data.Binary.Put (runPut)
import Data.ByteString (ByteString, empty)
import PotatoCactus.Game.Entity.Npc.Npc (Npc (forcedChat))
import PotatoCactus.Game.World (World)
import PotatoCactus.Network.Binary (encodeStr)

encodeNpcForcedChatBlock :: Npc -> World -> ByteString
encodeNpcForcedChatBlock npc _ =
  maybe empty encodeStr (forcedChat npc)
