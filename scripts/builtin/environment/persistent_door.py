from typing import Dict, Literal, Optional

from potato_cactus import Context, EventHandler, GameEvent
from potato_cactus.api.actions import (ClearPlayerInteraction,
                                       RemoveGameObject, SpawnGameObject)
from potato_cactus.api.dto.object import GameObject
from potato_cactus.api.events import ObjectInteractionEventPayload


@EventHandler(GameEvent.ObjectInteractionEvent,
              objectId=1519)  # Large door right
@EventHandler(GameEvent.ObjectInteractionEvent,
              objectId=1516)  # Large door left
@EventHandler(
    GameEvent.ObjectInteractionEvent, objectId=1551
)  # Gate right # TODO - handle both parts as one  - keotl 2023-05-13
@EventHandler(GameEvent.ObjectInteractionEvent, objectId=1553)  # Gate left
@EventHandler(GameEvent.ObjectInteractionEvent, objectId=1536)
@EventHandler(GameEvent.ObjectInteractionEvent, objectId=1530)
@EventHandler(GameEvent.ObjectInteractionEvent, objectId=1533)
def on_door_open(e: ObjectInteractionEventPayload, context: Context):
    if (e.interaction.target is None):
        return [ClearPlayerInteraction(e.playerIndex)]
    door = e.interaction.target.object
    return [
        SpawnGameObject(*matching_open_door(door, door_variant.get(door.id))),
        RemoveGameObject(door.position, door.objectType),
        ClearPlayerInteraction(e.playerIndex)
    ]


door_variant: Dict[int, Literal["left", "right"]] = {1516: "left"}


@EventHandler(GameEvent.ObjectInteractionEvent, objectId=1520)
@EventHandler(GameEvent.ObjectInteractionEvent, objectId=1531)
@EventHandler(GameEvent.ObjectInteractionEvent, objectId=1517)
@EventHandler(GameEvent.ObjectInteractionEvent, objectId=1552)  # Gate right
@EventHandler(GameEvent.ObjectInteractionEvent, objectId=1554)  # Gate left
@EventHandler(GameEvent.ObjectInteractionEvent, objectId=1534)
def on_door_close(e: ObjectInteractionEventPayload, context: Context):
    if (e.interaction.target is None):
        return [ClearPlayerInteraction(e.playerIndex)]
    door = e.interaction.target.object
    return [
        SpawnGameObject(
            *matching_closed_door(door, door_variant.get(door.id - 1))),
        RemoveGameObject(door.position, door.objectType),
        ClearPlayerInteraction(e.playerIndex)
    ]


def matching_open_door(closed_door: GameObject,
                       opening_style: Optional[Literal["left",
                                                       "right"]] = None):
    opening_style = opening_style or "right"
    new_facing = (closed_door.facingDirection +
                  (-1 if opening_style == "left" else 1)) % 4
    if closed_door.facingDirection == 0:
        # Opening towards W
        new_pos = (closed_door.position.x - 1, closed_door.position.y,
                   closed_door.position.z)
    elif closed_door.facingDirection == 1:
        # Opening towards N
        new_pos = (closed_door.position.x, closed_door.position.y + 1,
                   closed_door.position.z)
    elif closed_door.facingDirection == 2:
        # Opening towards E
        new_pos = (closed_door.position.x + 1, closed_door.position.y,
                   closed_door.position.z)
    else:
        # Opening towards S
        new_pos = (closed_door.position.x, closed_door.position.y - 1,
                   closed_door.position.z)
    return (closed_door.id + 1, new_pos, closed_door.objectType, new_facing)


def matching_closed_door(closed_door: GameObject,
                         opening_style: Optional[Literal["left",
                                                         "right"]] = None):
    opening_style = opening_style or "right"
    new_facing = (closed_door.facingDirection -
                  (-1 if opening_style == "left" else 1) + 4) % 4
    if new_facing == 0:
        # Opening towards W
        new_pos = (closed_door.position.x + 1, closed_door.position.y,
                   closed_door.position.z)
    elif new_facing == 1:
        # Opening towards N
        new_pos = (closed_door.position.x, closed_door.position.y - 1,
                   closed_door.position.z)
    elif new_facing == 2:
        # Opening towards E
        new_pos = (closed_door.position.x - 1, closed_door.position.y,
                   closed_door.position.z)
    else:
        # Opening towards S
        new_pos = (closed_door.position.x, closed_door.position.y + 1,
                   closed_door.position.z)
    return (closed_door.id - 1, new_pos, closed_door.objectType, new_facing)
