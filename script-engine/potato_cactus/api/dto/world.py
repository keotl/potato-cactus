from abc import ABC
from typing import List

from potato_cactus.api.dto.npc import Npc
from potato_cactus.api.dto.player import Player


class World(ABC):
    tick: int
    players: List[Player]
    npcs: List[Npc]
