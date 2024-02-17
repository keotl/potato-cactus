from potato_cactus import Context, EventHandler, GameEvent
from potato_cactus.api.actions import SendMessage, SetPlayerPosition
from potato_cactus.api.events import PlayerCommandEventPayload


@EventHandler(GameEvent.PlayerCommandEvent, command="teleport")
@EventHandler(GameEvent.PlayerCommandEvent, command="tp")
def on_command(e: PlayerCommandEventPayload, context: Context):
    player = context.find_player_by_index(e.playerIndex)

    if len(e.args) < 2:
        return [
            SendMessage(e.playerIndex, f"Usage: ::{e.command} <x> <y> [z]")
        ]
    x = int(e.args[0])
    y = int(e.args[1])
    z = int(e.args[2]) if len(e.args) > 2 else player.movement.position.z

    return [
        SetPlayerPosition(
            e.playerIndex,
            (x, y, z),
        ),
        SendMessage(
            e.playerIndex,
            f"Teleported to ({x}, {y}, {z})",
        )
    ]
