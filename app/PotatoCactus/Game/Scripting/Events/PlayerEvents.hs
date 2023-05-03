module PotatoCactus.Game.Scripting.Events.PlayerEvents (createPlayerEvents) where

import Data.Maybe (catMaybes)
import PotatoCactus.Game.Combat.CombatEntity (CombatEntity (cooldown), CombatTarget (None))
import qualified PotatoCactus.Game.Combat.CombatEntity as Combat
import PotatoCactus.Game.Entity.Interaction.Interaction (Interaction (state))
import PotatoCactus.Game.Entity.Interaction.State (InteractionState (InProgress))
import qualified PotatoCactus.Game.Interface.InterfaceController as IC
import PotatoCactus.Game.Player (Player (Player, combat, interaction, interfaces))
import PotatoCactus.Game.Scripting.ScriptUpdates (GameEvent (PlayerAttackEvent, PlayerInteractionEvent, ScriptInvokedEvent))

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

interfaceEvents_ :: Player -> [GameEvent]
interfaceEvents_ Player {interfaces = ic} =
  map ScriptInvokedEvent (IC.triggeredCallbacks ic)
