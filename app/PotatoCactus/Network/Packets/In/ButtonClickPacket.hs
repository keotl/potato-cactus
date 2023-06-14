module PotatoCactus.Network.Packets.In.ButtonClickPacket where

import Data.Binary.Get (getWord16be, runGet)
import Data.ByteString.Lazy (fromStrict)
import PotatoCactus.Game.Message.GameChannelMessage (GameChannelMessage (InterfaceButtonClickMessage))
import PotatoCactus.Network.Packets.Reader (InboundPacket (payload))
import PotatoCactus.Game.Player (PlayerIndex)

buttonClickMessage :: PlayerIndex -> InboundPacket -> GameChannelMessage
buttonClickMessage playerId packet =
  InterfaceButtonClickMessage
    playerId
    $ fromIntegral
      ( runGet
          getWord16be
          (fromStrict $ payload packet)
      )
