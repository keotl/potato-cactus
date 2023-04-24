from enum import Enum
from typing import Optional

from potato_cactus.api.dto.combat import CombatTarget
from potato_cactus.api.dto.interaction import PlayerInteraction


class GameEvent(str, Enum):
    NpcEntityTickEvent = "NpcEntityTickEvent"
    PlayerInteractionEvent = "PlayerInteractionEvent"
    PlayerAttackEvent = "PlayerAttackEvent"
    NpcAttackEvent = "NpcAttackEvent"
    NpcDeadEvent = "NpcDeadEvent"


class PlayerInteractionEventPayload(object):
    playerIndex: int
    interaction: PlayerInteraction


class PlayerAttackEventPayload(object):
    playerIndex: int
    target: Optional[CombatTarget]


class NpcAttackEventPayload(object):
    npcIndex: int
    target: Optional[CombatTarget]


class NpcReference(object):
    npcIndex: int


NpcDeadEventPayload = NpcReference
NpcEntityTickEventPayload = NpcReference
