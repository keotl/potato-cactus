from potato_cactus import EventHandler, GameEvent, Context
from potato_cactus.api.actions import SetPlayerPosition, SendMessage
from potato_cactus.api.events import PlayerCommandEventPayload


@EventHandler(GameEvent.PlayerCommandEvent, command="teleport")
@EventHandler(GameEvent.PlayerCommandEvent, command="tp")
def on_command(e: PlayerCommandEventPayload, context: Context):
    player = context.find_player_by_index(e.playerIndex)
    if player is None:
        return []
    if len(e.args) < 3:
        return [SendMessage(e.playerIndex, f"Usage: ::{e.command} <x> <y> <z>")]
    return [SetPlayerPosition(e.playerIndex, (int(e.args[0]), int(e.args[1]), int(e.args[2])))]
