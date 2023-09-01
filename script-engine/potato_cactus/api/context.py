from abc import ABC, abstractmethod
from typing import Optional

from potato_cactus.api.dto.npc import Npc
from potato_cactus.api.dto.object import GameObject
from potato_cactus.api.dto.player import Player
from potato_cactus.api.dto.position import Position
from potato_cactus.api.dto.world import World


class Context(ABC):
    world: World

    def find_player_by_index(self, player_index: int) -> Optional[Player]:
        if player_index >= len(self.world.players):
            return None
        return self.world.players[player_index]

    def find_npc_by_index(self, npc_index: int) -> Optional[Npc]:
        if npc_index >= len(self.world.npcs):
            return None
        return self.world.npcs[npc_index]

    @abstractmethod
    def find_static_game_object(self, position: Position,
                                objType: int) -> Optional[GameObject]:
        raise NotImplementedError
