from potato_cactus import Context, EventHandler, GameEvent
from potato_cactus.api.actions import RemoveItemStack, SpawnGroundItem
from potato_cactus.api.events import DropItemEventPayload


@EventHandler(GameEvent.DropItemEvent, itemId="unhandled")
def drop_item_default_handler(e: DropItemEventPayload, ctx: Context):
    player = ctx.find_player_by_index(e.playerIndex)
    stack = player.inventory[e.index]
    if stack is None:
        return []

    return [
        RemoveItemStack(e.playerIndex, e.itemId, e.index),
        SpawnGroundItem(e.itemId, stack.quantity, player.movement.position,
                        player.username)
    ]
