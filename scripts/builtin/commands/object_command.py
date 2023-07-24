from potato_cactus import Context, EventHandler, GameEvent
from potato_cactus.api.actions import SendMessage, SpawnGameObject
from potato_cactus.api.events import PlayerCommandEventPayload


@EventHandler(GameEvent.PlayerCommandEvent, command="obj")
@EventHandler(GameEvent.PlayerCommandEvent, command="object")
@EventHandler(GameEvent.PlayerCommandEvent, command="spawnObject")
def on_command(e: PlayerCommandEventPayload, context: Context):
    player = context.find_player_by_index(e.playerIndex)
    if player is None:
        return []
    if len(e.args) < 1:
        return [SendMessage(e.playerIndex, f"Usage: ::{e.command} <objectId> [objectType]")]
    object_type = 10 if len(e.args) < 2 else int(e.args[1])
    return [SpawnGameObject(int(e.args[0]), player.movement.position, object_type, 0)]

