from typing import Generic, TypeVar, Literal, Optional

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


class NpcAttackInteractionTarget(object):
    type: Literal["npcAttack"]
    npcIndex: int


class NpcInteractionTarget(object):
    type: Literal["npc"]
    npcIndex: int
    actionIndex: int
