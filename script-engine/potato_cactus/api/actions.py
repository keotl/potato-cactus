from typing import Dict, List, Literal, Optional, Tuple, Union

from potato_cactus.api.dto.interface import InterfaceElement
from potato_cactus.api.dto.position import Position
from potato_cactus.api.dto.script_invocation import ScriptInvocation


class ScriptAction(object):
    op: str
    body: dict

    def __init__(self, op, body):
        self.op = op
        self.body = body


def ClearPlayerInteraction(playerIndex: int) -> ScriptAction:
    return ScriptAction("clearPlayerInteraction", {"playerIndex": playerIndex})


def ServerPrintMessage(msg: str) -> ScriptAction:
    return ScriptAction("serverPrintMessage", {"msg": msg})


def NpcQueueWalk(
        npcIndex: int, position: Union[Position, Tuple[int, int,
                                                       int]]) -> ScriptAction:
    return ScriptAction("npcQueueWalk", {
        "npcIndex": npcIndex,
        "position": _map_position(position)
    })


def NpcSetAnimation(
        npcIndex: int,
        animationId: int,
        delay: int = 0,
        priority: Literal["high", "normal", "low"] = "normal") -> ScriptAction:
    return ScriptAction(
        "npcSetAnimation", {
            "npcIndex": npcIndex,
            "animationId": animationId,
            "delay": delay,
            "priority": priority
        })


def NpcSetForcedChat(npcIndex: int, message: str) -> ScriptAction:
    return ScriptAction("npcSetForcedChat", {
        "npcIndex": npcIndex,
        "message": message
    })


def SpawnNpc(npcId: int,
             position: Union[Position, Tuple[int, int, int]],
             respawnDelay=None) -> ScriptAction:
    return ScriptAction(
        "spawnNpc", {
            "npcId": npcId,
            "position": _map_position(position),
            "respawnDelay": respawnDelay or -1
        })


def SendMessage(playerIndex: int, text: str) -> ScriptAction:
    return ScriptAction("sendMessage", {
        "playerIndex": playerIndex,
        "text": text
    })


def SetPlayerPosition(
        playerIndex: int,
        position: Union[Position, Tuple[int, int, int]]) -> ScriptAction:
    return ScriptAction("setPlayerPosition", {
        "playerIndex": playerIndex,
        "position": _map_position(position)
    })


def InvokeScript(callback: ScriptInvocation) -> ScriptAction:
    return ScriptAction("invokeScript", callback.__dict__)


def CreateInterface(
        playerIndex: int,
        type: Literal["standard", "input", "walkable"],
        elements: List[InterfaceElement],
        onClose: Optional[ScriptInvocation] = None,
        callbacks: Optional[Dict[int,
                                 ScriptInvocation]] = None) -> ScriptAction:
    return ScriptAction(
        "createInterface", {
            "type":
                type,
            "playerIndex":
                playerIndex,
            "elements":
                list(map(lambda e: e.serialize(), elements)),
            "onClose":
                onClose.__dict__ if onClose else None,
            "callbacks": [(k, v.__dict__)
                          for k, v in callbacks.items()] if callbacks else []
        })


def ClearStandardInterface(playerIndex: int) -> ScriptAction:
    return ScriptAction("clearStandardInterface", {"playerIndex": playerIndex})


def SetPlayerEntityData(playerIndex: int, key: str,
                        val: Union[int, str, bool]) -> ScriptAction:
    return ScriptAction("setPlayerEntityData", {
        "playerIndex": playerIndex,
        "key": key,
        "val": val
    })


def _map_position(position: Union[Position, Tuple[int, int, int]]) -> dict:
    if isinstance(position, Position):
        return {"x": position.x, "y": position.y, "z": position.z}
    return {"x": position[0], "y": position[1], "z": position[2]}
