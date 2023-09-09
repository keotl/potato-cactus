from potato_cactus import EventHandler, GameEvent
from potato_cactus.api.actions import (ClearPlayerInteraction, GiveItem,
                                       InvokeScript, RemoveGameObject,
                                       SetPlayerAnimation, SpawnGameObject)
from potato_cactus.api.dto.script_invocation import ScriptInvocation
from potato_cactus.api.events import ObjectInteractionEventPayload


@EventHandler(GameEvent.ObjectInteractionEvent, objectId=313)
@EventHandler(GameEvent.ObjectInteractionEvent, objectId=5585)
@EventHandler(GameEvent.ObjectInteractionEvent, objectId=5584)
def on_interaction(e: ObjectInteractionEventPayload):
    if e.interaction.target is None:
        return [ClearPlayerInteraction(e.playerIndex)]

    target = e.interaction.target.object
    return [
        GiveItem(e.playerIndex, 1947, 1),
        SetPlayerAnimation(e.playerIndex, 827),
        RemoveGameObject(target.position, target.objectType),
        ClearPlayerInteraction(e.playerIndex),
        InvokeScript(
            ScriptInvocation(respawn, (target.id, target.position.x,
                                       target.position.y, target.position.z)),
            RESPAWN_DELAY)
    ]


RESPAWN_DELAY = 10


def respawn(objectId: int, x: int, y: int, z: int):
    return [SpawnGameObject(objectId, (x, y, z), 10, 0)]
