from potato_cactus import Context, EventHandler
from potato_cactus.api.actions import SendMessage
from potato_cactus.api.events import GameEvent, PlayerCommandEventPayload


@EventHandler(GameEvent.PlayerCommandEvent, command="position")
@EventHandler(GameEvent.PlayerCommandEvent, command="pos")
def on_command(e: PlayerCommandEventPayload, context: Context):
    player = context.find_player_by_index(e.playerIndex)
    if player is None:
        return []

    return [
        SendMessage(
            e.playerIndex,
            f"Position [{player.movement.position.x}, {player.movement.position.y}, {player.movement.position.z}]"
        )
    ]
