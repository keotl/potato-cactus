from potato_cactus import Context, EventHandler, GameEvent
from potato_cactus.api.actions import GiveItem, SendMessage
from potato_cactus.api.events import PlayerCommandEventPayload


@EventHandler(GameEvent.PlayerCommandEvent, command="item")
def on_command(e: PlayerCommandEventPayload, context: Context):
    player = context.find_player_by_index(e.playerIndex)
    if player is None:
        return []
    if len(e.args) < 1:
        return [SendMessage(e.playerIndex, f"Usage: ::{e.command} <itemId> [quantity]")]
    quantity = 1
    item_id = int(e.args[0])
    if len(e.args) >= 2:
        quantity = int(e.args[1])
    return [
        GiveItem(e.playerIndex, item_id, quantity)
    ]
