from typing import Literal
from potato_cactus.api.dto.position import Position
from potato_cactus.api.dto.object import GameObject


class ScriptAction(object):
    op: str
    body: dict

    def __init__(self, op, body):
        self.op = op
        self.body = body


def ClearPlayerInteraction(playerIndex: int) -> ScriptAction:
    return ScriptAction("clearPlayerInteraction", {"playerIndex": playerIndex})


def DummyEvent(key: str) -> ScriptAction:
    return ScriptAction("dummyEvent", {"key": key})


def NpcQueueWalk(npcIndex: int, position: Position) -> ScriptAction:
    return ScriptAction("npcQueueWalk", {"npcIndex": npcIndex,
                                         "position": _map_position(position)})


def SpawnGameObject(obj: GameObject) -> ScriptAction:  # AddGameObject [Added]
    return ScriptAction("addGameObject", {
        "op": "add",
        "id": obj.id,
        "position": _map_position(obj.position),
        "objectType": obj.objectType,
        "facingDirection": obj.facingDirection
    })


def RemoveGameObject(obj: GameObject) -> ScriptAction:  # AddGameObject [Removed]
    return ScriptAction("addGameObject", {
        "op": "remove",
        "id": obj.id,
        "position": _map_position(obj.position),
        "objectType": obj.objectType,
        "facingDirection": obj.facingDirection
    })


def NpcSetAnimation(npcIndex: int, animationId: int, delay: int = 0,
                    priority: Literal["high", "normal", "low"] = "normal") -> ScriptAction:
    return ScriptAction("npcSetAnimation", {
        "npcIndex": npcIndex,
        "animationId": animationId,
        "delay": delay,
        "priority": priority
    })


def _map_position(position: Position) -> dict:
    return {"x": position.x, "y": position.y, "z": position.z}
