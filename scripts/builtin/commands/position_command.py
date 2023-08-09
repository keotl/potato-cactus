from potato_cactus import Context, EventHandler
from potato_cactus.api.actions import SendMessage
from potato_cactus.api.events import GameEvent, PlayerCommandEventPayload


@EventHandler(GameEvent.PlayerCommandEvent, command="position")
@EventHandler(GameEvent.PlayerCommandEvent, command="pos")
def on_command(e: PlayerCommandEventPayload, context: Context):
    player = context.find_player_by_index(e.playerIndex)
    if player is None:
        return []
    if len(e.args) > 0 and e.args[0] in ("-v", "--verbose"):
        return [
            SendMessage(e.playerIndex, f"Position [{player.movement.position.x}, {player.movement.position.y}, {player.movement.position.z}]"),
            SendMessage(e.playerIndex, f"Chunk ({player.movement.position.x // 8},{player.movement.position.y // 8})"),
            SendMessage(e.playerIndex, f"Map ({player.movement.position.x // 64},{player.movement.position.y // 64}); Offset ({player.movement.position.x - (player.movement.position.x // 64) * 64},{player.movement.position.y - (player.movement.position.y // 64) * 64})"),
        ]
    return [
        SendMessage(
            e.playerIndex,
            f"Position [{player.movement.position.x}, {player.movement.position.y}, {player.movement.position.z}]"
        )
    ]
