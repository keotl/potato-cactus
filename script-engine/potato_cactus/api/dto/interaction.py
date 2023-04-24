from typing import Union, Literal, Optional

from potato_cactus.api.dto.position import Position


class InteractionTarget(object):
    type: Literal["object", "npcAttack", "npc"]
    objectId: Optional[int]
    npcIndex: Optional[int]
    position: Optional[Position]
    actionIndex: Optional[int]


class PlayerInteraction(object):
    target: Optional[InteractionTarget]
    state: Literal["pending", "pendingPathing", "inProgress"]
