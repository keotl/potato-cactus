module PotatoCactus.Network.Packets.In.ContinueDialoguePacket where

import Data.Binary.Get (getWord16be, runGet)
import Data.ByteString.Lazy (fromStrict)
import PotatoCactus.Game.Message.GameChannelMessage (GameChannelMessage (PlayerContinueDialogueMessage))
import PotatoCactus.Game.Player (PlayerIndex)
import PotatoCactus.Network.Packets.Reader (InboundPacket (InboundPacket, payload))

continueDialoguePacket :: PlayerIndex -> InboundPacket -> GameChannelMessage
continueDialoguePacket playerIndex packet =
  PlayerContinueDialogueMessage
    playerIndex
    $ fromIntegral
      ( runGet
          getWord16be
          (fromStrict $ payload packet)
      )
