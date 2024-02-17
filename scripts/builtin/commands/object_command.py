from potato_cactus import Context, EventHandler, GameEvent
from potato_cactus.api.actions import (RemoveGameObject, SendMessage,
                                       SpawnGameObject)
from potato_cactus.api.events import PlayerCommandEventPayload


@EventHandler(GameEvent.PlayerCommandEvent, command="obj")
@EventHandler(GameEvent.PlayerCommandEvent, command="object")
@EventHandler(GameEvent.PlayerCommandEvent, command="spawnObject")
def on_command(e: PlayerCommandEventPayload, context: Context):
    player = context.find_player_by_index(e.playerIndex)
    if len(e.args) < 1:
        return [
            SendMessage(
                e.playerIndex,
                f"Usage: ::{e.command} <objectId> [objectType] [orientation]")
        ]
    object_type = 10 if len(e.args) < 2 else int(e.args[1])
    orientation = 0 if len(e.args) < 3 else int(e.args[2])
    return [
        SpawnGameObject(int(e.args[0]), player.movement.position, object_type,
                        orientation),
        SendMessage(
            e.playerIndex,
            f"Spawn object id={e.args[0]} type={object_type} orientation={orientation}"
        )
    ]


@EventHandler(GameEvent.PlayerCommandEvent, command="removeobject")
@EventHandler(GameEvent.PlayerCommandEvent, command="rmobj")
def on_remove_object_command(e: PlayerCommandEventPayload, context: Context):
    player = context.find_player_by_index(e.playerIndex)
    if len(e.args) < 1:
        return [
            SendMessage(e.playerIndex, f"Usage: ::{e.command} <objectType>")
        ]
    return [
        RemoveGameObject(player.movement.position, int(e.args[0])),
        SendMessage(e.playerIndex, f"Removed object type={e.args[0]}")
    ]
