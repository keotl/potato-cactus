module PotatoCactus.Game.Scripting.Events.PlayerEvents (createPlayerEvents) where

import Data.Maybe (catMaybes)
import PotatoCactus.Game.Entity.Interaction.Interaction (Interaction (state))
import PotatoCactus.Game.Entity.Interaction.State (InteractionState (InProgress))
import PotatoCactus.Game.Player (Player (interaction))
import PotatoCactus.Game.Scripting.ScriptUpdates (GameEvent (PlayerInteraction))

createPlayerEvents :: Player -> [GameEvent]
createPlayerEvents player =
  catMaybes
    [ interactionEvent_ player
    ]

interactionEvent_ :: Player -> Maybe GameEvent
interactionEvent_ p =
  case state . interaction $ p of
    InProgress -> Just $ PlayerInteraction p (interaction p)
    _ -> Nothing
