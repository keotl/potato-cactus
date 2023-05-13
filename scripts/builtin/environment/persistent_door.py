from potato_cactus import Context, EventHandler, GameEvent
from potato_cactus.api.actions import ClearPlayerInteraction, SpawnGameObject
from potato_cactus.api.events import ObjectInteractionEventPayload


@EventHandler(GameEvent.ObjectInteractionEvent, objectId=1519)
@EventHandler(GameEvent.ObjectInteractionEvent, objectId=1516)
def on_door_open(e: ObjectInteractionEventPayload, context: Context):
    if (e.interaction.target is None):
        return [ClearPlayerInteraction(e.playerIndex)]

    return [
        SpawnGameObject(
            e.interaction.target.objectId + 1, e.interaction.target.position,
            0, 2
        ),  # TODO - Calculate facing from static object set  - keotl 2023-05-13
        ClearPlayerInteraction(e.playerIndex)
    ]


@EventHandler(GameEvent.ObjectInteractionEvent, objectId=1520)
@EventHandler(GameEvent.ObjectInteractionEvent, objectId=1517)
def on_door_close(e: ObjectInteractionEventPayload, context: Context):
    if (e.interaction.target is None):
        return [ClearPlayerInteraction(e.playerIndex)]

    return [
        SpawnGameObject(
            e.interaction.target.objectId - 1, e.interaction.target.position,
            0, 1
        ),  # TODO - Calculate facing from static object set  - keotl 2023-05-13
        ClearPlayerInteraction(e.playerIndex)
    ]
