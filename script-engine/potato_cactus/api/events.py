from enum import Enum
from typing import List, Optional

from potato_cactus.api.dto.combat import CombatTarget
from potato_cactus.api.dto.interaction import (GroundItemInteractionTarget,
                                               ItemOnObjectInteractionTarget,
                                               NpcInteractionTarget,
                                               ObjectInteractionTarget,
                                               PlayerInteraction)


class GameEvent(str, Enum):
    ServerInitEvent = "ServerInitEvent"
    NpcEntityTickEvent = "NpcEntityTickEvent"
    NpcInteractionEvent = "NpcInteractionEvent"
    ObjectInteractionEvent = "ObjectInteractionEvent"
    ItemOnObjectInteractionEvent = "ItemOnObjectInteractionEvent"
    PickupItemInteractionEvent = "PickupItemInteractionEvent"
    PlayerAttackEvent = "PlayerAttackEvent"
    PlayerCommandEvent = "PlayerCommandEvent"
    NpcAttackEvent = "NpcAttackEvent"
    NpcDeadEvent = "NpcDeadEvent"
    DropItemEvent = "DropItemEvent"


class ObjectInteractionEventPayload(object):
    playerIndex: int
    interaction: PlayerInteraction[ObjectInteractionTarget]


class ItemOnObjectInteractionEventPayload(object):
    playerIndex: int
    interaction: PlayerInteraction[ItemOnObjectInteractionTarget]


class NpcInteractionEventPayload(object):
    playerIndex: int
    interaction: PlayerInteraction[NpcInteractionTarget]


class PickupItemInteractionEventPayload(object):
    playerIndex: int
    interaction: PlayerInteraction[GroundItemInteractionTarget]


class PlayerAttackEventPayload(object):
    playerIndex: int
    target: Optional[CombatTarget]


class PlayerCommandEventPayload(object):
    playerIndex: int
    command: str
    args: List[str]


class NpcAttackEventPayload(object):
    npcIndex: int
    target: Optional[CombatTarget]


class DropItemEventPayload(object):
    playerIndex: int
    widgetId: int
    itemId: int
    index: int


class NpcReference(object):
    npcIndex: int


NpcDeadEventPayload = NpcReference
NpcEntityTickEventPayload = NpcReference
