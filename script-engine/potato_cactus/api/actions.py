from typing import Callable, Dict, List, Literal, Optional, Tuple, Union

from potato_cactus import get_context
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


def SetPlayerAnimation(
        playerIndex: int,
        animationId: int,
        delay: int = 0,
        priority: Literal["high", "normal", "low"] = "normal") -> ScriptAction:
    return ScriptAction(
        "setPlayerAnimation", {
            "playerIndex": playerIndex,
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


def SpawnGameObject(objectId: int, position: Union[Position, Tuple[int, int,
                                                                   int]],
                    objectType: int, facingDirection: int) -> ScriptAction:
    return ScriptAction(
        "spawnObject", {
            "objectId": objectId,
            "position": _map_position(position),
            "objectType": objectType,
            "facingDirection": facingDirection
        })


def RemoveGameObject(objectId: int,
                     position: Union[Position, Tuple[int, int, int]],
                     objectType: int,
                     facingDirection: int = 0) -> ScriptAction:
    return ScriptAction(
        "removeObject", {
            "objectId": objectId,
            "position": _map_position(position),
            "objectType": objectType,
            "facingDirection": facingDirection
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


def InvokeScript(callback: Union[Callable[[], List[ScriptAction]],
                                 ScriptInvocation],
                 delay: int = 1) -> ScriptAction:
    if isinstance(callback, Callable):
        callback = ScriptInvocation(callback)

    return ScriptAction("invokeScript", {
        "f": callback.f,
        "args": callback.args,
        "delay": delay
    })


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


def GiveItem(playerIndex: int, itemId: int, quantity: int = 1) -> ScriptAction:
    return ScriptAction("giveItem", {
        "playerIndex": playerIndex,
        "itemId": itemId,
        "quantity": quantity
    })


def SubtractItem(playerIndex: int,
                 itemId: int,
                 quantity: int = 1) -> ScriptAction:
    return ScriptAction("subtractItem", {
        "playerIndex": playerIndex,
        "itemId": itemId,
        "quantity": quantity
    })


def RemoveItemStack(playerIndex: int, itemId: int, index: int) -> ScriptAction:
    return ScriptAction("removeItemStack", {
        "playerIndex": playerIndex,
        "itemId": itemId,
        "index": index
    })


def SpawnGroundItem(itemId: int,
                    quantity: int,
                    position: Union[Position, Tuple[int, int, int]],
                    player: Optional[str] = None,
                    despawn_delay: int = 100) -> ScriptAction:
    return ScriptAction(
        "spawnGroundItem", {
            "itemId": itemId,
            "quantity": quantity,
            "position": _map_position(position),
            "player": player,
            "despawnTime": get_context().world.tick + despawn_delay
        })


def RemoveGroundItem(itemId: int,
                     quantity: int,
                     position: Union[Position, Tuple[int, int, int]],
                     removedByPlayer: Optional[int] = None) -> ScriptAction:
    return ScriptAction(
        "removeGroundItem",
        {
            "itemId": itemId,
            "quantity": quantity,
            "position": _map_position(position),
            "removedByPlayer":
                removedByPlayer  # For removing scoped items and giving to player
        })


def SetVarp(playerIndex: int, varpId: int, value: int) -> ScriptAction:
    return ScriptAction("setVarp", {
        "playerIndex": playerIndex,
        "varpId": varpId,
        "value": value
    })


def SetVarbit(playerIndex: int, varpId: int, msb: int, length: int,
              value: int) -> ScriptAction:
    return ScriptAction(
        "setVarp", {
            "playerIndex": playerIndex,
            "varpId": varpId,
            "msb": msb,
            "length": length,
            "value": value
        })


def _map_position(position: Union[Position, Tuple[int, int, int]]) -> dict:
    if hasattr(position, "x") or isinstance(position, Position):
        return {
            "x": position.x,  # type: ignore
            "y": position.y,  # type: ignore
            "z":
                position.z  # type: ignore
        }

    return {"x": position[0], "y": position[1], "z": position[2]}
