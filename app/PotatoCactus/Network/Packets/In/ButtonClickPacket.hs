module PotatoCactus.Network.Packets.In.ButtonClickPacket where

import Data.Binary.Get (getWord16be, runGet)
import Data.ByteString.Lazy (fromStrict)
import PotatoCactus.Boot.GameChannel (GameChannelMessage (InterfaceButtonClickMessage))
import PotatoCactus.Network.Packets.Reader (InboundPacket (payload))

buttonClickMessage :: String -> InboundPacket -> GameChannelMessage
buttonClickMessage username packet =
  InterfaceButtonClickMessage
    username
    $ fromIntegral
      ( runGet
          getWord16be
          (fromStrict $ payload packet)
      )
