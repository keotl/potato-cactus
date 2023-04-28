from typing import Literal, Optional


class CombatTarget(object):
    type: Literal["player", "npc"]
    playerIndex: Optional[int]
    npcIndex: Optional[int]


class Combat(object):
    hitpoints: int
    state: Literal["alive", "dying", "dead"]
    maxHitpoints: int
    target: CombatTarget
    cooldown: int
