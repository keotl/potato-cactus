module PotatoCactus.Game.Scripting.Events.PlayerEvents (createPlayerEvents) where

import Data.Maybe (catMaybes, mapMaybe)
import PotatoCactus.Game.Combat.CombatEntity (CombatEntity (cooldown), CombatTarget (None))
import qualified PotatoCactus.Game.Combat.CombatEntity as Combat
import PotatoCactus.Game.Entity.Interaction.Interaction (Interaction (state))
import qualified PotatoCactus.Game.Entity.Interaction.Interaction as Interaction
import PotatoCactus.Game.Entity.Interaction.State (InteractionState (InProgress, PendingPathing))
import qualified PotatoCactus.Game.Interface.InterfaceController as IC
import qualified PotatoCactus.Game.ItemContainer as ItemContainer
import PotatoCactus.Game.Player (Player (Player, combat, interaction, interfaces))
import qualified PotatoCactus.Game.Player as P
import PotatoCactus.Game.Scripting.ScriptUpdates (GameEvent (DropItemEvent, InternalPlayerInteractionPendingPathingEvent, PlayerAttackEvent, PlayerInteractionEvent, ScriptInvokedEvent))

createPlayerEvents :: Player -> [GameEvent]
createPlayerEvents player =
  catMaybes
    [ interactionEvent_ player,
      attackEvent_ player
    ]
    ++ interfaceEvents_ player
    ++ dropItemEvents_ player

interactionEvent_ :: Player -> Maybe GameEvent
interactionEvent_ p =
  case state . interaction $ p of
    InProgress -> Just $ PlayerInteractionEvent p (interaction p)
    PendingPathing ->
      Just $
        InternalPlayerInteractionPendingPathingEvent
          p
          (Interaction.target . interaction $ p)
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

dropItemEvents_ :: Player -> [GameEvent]
dropItemEvents_ p =
  mapMaybe
    ( \itemIndex ->
        case ItemContainer.atIndex itemIndex (P.inventory p) of
          ItemContainer.Empty -> Nothing
          stack ->
            Just
              ( DropItemEvent
                  (P.serverIndex p)
                  (ItemContainer.widgetId . P.inventory $ p)
                  (ItemContainer.itemId stack)
                  itemIndex
              )
    )
    (P.droppedItemIndices p)
