from typing import Generic, Literal, Optional, TypeVar

from potato_cactus.api.dto.position import Position

T = TypeVar("T")


class PlayerInteraction(Generic[T]):
    target: Optional[T]
    state: Literal["pending", "pendingPathing", "inProgress"]


class ObjectInteractionTarget(object):
    type: Literal["object"]
    objectId: int
    position: Position
    actionIndex: int


class ItemOnObjectInteractionTarget(object):
    type: Literal["itemOnObject"]
    objectId: int
    position: Position
    itemId: int
    itemIndex: int
    interfaceId: int


class NpcAttackInteractionTarget(object):
    type: Literal["npcAttack"]
    npcIndex: int


class NpcInteractionTarget(object):
    type: Literal["npc"]
    npcIndex: int
    actionIndex: int


class GroundItemInteractionTarget(object):
    type: Literal["groundItem"]
    itemId: int
    position: Position
    quantity: int
