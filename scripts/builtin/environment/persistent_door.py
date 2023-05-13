from potato_cactus import Context, EventHandler, GameEvent
from potato_cactus.api.actions import ClearPlayerInteraction, SpawnGameObject
from potato_cactus.api.events import ObjectInteractionEventPayload


@EventHandler(GameEvent.ObjectInteractionEvent, objectId=1519) # Large door right
@EventHandler(GameEvent.ObjectInteractionEvent, objectId=1516) # Large door left
@EventHandler(GameEvent.ObjectInteractionEvent, objectId=1551) # Gate right # TODO - handle both parts as one  - keotl 2023-05-13
@EventHandler(GameEvent.ObjectInteractionEvent, objectId=1553) # Gate left
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
@EventHandler(GameEvent.ObjectInteractionEvent, objectId=1552) # Gate right
@EventHandler(GameEvent.ObjectInteractionEvent, objectId=1554) # Gate left
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
