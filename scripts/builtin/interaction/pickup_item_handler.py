from potato_cactus.api.actions import ClearPlayerInteraction, RemoveGroundItem
from potato_cactus.api.context import Context
from potato_cactus.api.events import (GameEvent,
                                      PickupItemInteractionEventPayload)
from potato_cactus.internal.registry import EventHandler


@EventHandler(GameEvent.PickupItemInteractionEvent, itemId="default")
def on_pickup_item(e: PickupItemInteractionEventPayload, ctx: Context):
    player = ctx.find_player_by_index(e.playerIndex)

    if player is None or e.interaction.target is None:
        return [ClearPlayerInteraction(e.playerIndex)]

    return [
        RemoveGroundItem(e.interaction.target.itemId,
                         e.interaction.target.quantity,
                         e.interaction.target.position,
                         removedByPlayer=e.playerIndex),
        ClearPlayerInteraction(e.playerIndex)
    ]
