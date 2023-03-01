module PotatoCactus.Game.Message.RegisterClientPayload where

import PotatoCactus.Game.Player (Player (Player))
import PotatoCactus.Game.World (ClientHandle (ClientHandle))

data RegisterClientPayload = RegisterClientPayload
  { clientHandle :: ClientHandle,
    player :: Player
  }
