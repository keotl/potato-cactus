module PotatoCactus.Game.Scripting.Events.PlayerEvents (createPlayerEvents) where

import Data.Maybe (catMaybes)
import PotatoCactus.Game.Combat.CombatEntity (CombatEntity (cooldown), CombatTarget (None))
import qualified PotatoCactus.Game.Combat.CombatEntity as Combat
import PotatoCactus.Game.Entity.Interaction.Interaction (Interaction (state))
import PotatoCactus.Game.Entity.Interaction.State (InteractionState (InProgress))
import PotatoCactus.Game.Player (Player (combat, interaction))
import PotatoCactus.Game.Scripting.ScriptUpdates (GameEvent (PlayerAttackEvent, PlayerInteractionEvent))

createPlayerEvents :: Player -> [GameEvent]
createPlayerEvents player =
  catMaybes
    [ interactionEvent_ player,
      attackEvent_ player
    ]

interactionEvent_ :: Player -> Maybe GameEvent
interactionEvent_ p =
  case state . interaction $ p of
    InProgress -> Just $ PlayerInteractionEvent p (interaction p)
    _ -> Nothing

attackEvent_ :: Player -> Maybe GameEvent
attackEvent_ p =
  case Combat.target . combat $ p of
    None -> Nothing
    target ->
      if 0 == (cooldown . combat $ p)
        then Just $ PlayerAttackEvent p target
        else Nothing
