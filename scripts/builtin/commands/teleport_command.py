from potato_cactus import Context, EventHandler, GameEvent
from potato_cactus.api.actions import SendMessage, SetPlayerPosition
from potato_cactus.api.events import PlayerCommandEventPayload


@EventHandler(GameEvent.PlayerCommandEvent, command="teleport")
@EventHandler(GameEvent.PlayerCommandEvent, command="tp")
def on_command(e: PlayerCommandEventPayload, context: Context):
    player = context.find_player_by_index(e.playerIndex)
    if player is None:
        return []
    if len(e.args) < 3:
        return [
            SendMessage(e.playerIndex, f"Usage: ::{e.command} <x> <y> <z>")
        ]
    return [
        SetPlayerPosition(
            e.playerIndex,
            (int(e.args[0]), int(e.args[1]), int(e.args[2])),
        ),
        SendMessage(
            e.playerIndex,
            f"Teleported to ({e.args[0]}, {e.args[1]}, {e.args[2]})",
        )
    ]
