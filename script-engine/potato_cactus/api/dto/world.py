from abc import ABC
from typing import List, Optional

from potato_cactus.api.dto.npc import Npc
from potato_cactus.api.dto.player import Player


class World(ABC):
    tick: int
    players: List[Optional[Player]]
    npcs: List[Optional[Npc]]

