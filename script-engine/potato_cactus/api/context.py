from abc import ABC
from typing import Optional

from potato_cactus.api.dto.npc import Npc
from potato_cactus.api.dto.player import Player
from potato_cactus.api.dto.world import World


class Context(ABC):
    world: World

    def maybe_find_player_by_index(self,
                                   player_index: int) -> Optional[Player]:
        """Returns an Optional[Player]. Use this option if the player
        is likely to be missing to avoid the performance overhead of
        exception handling."""
        if player_index >= len(self.world.players):
            return None
        return self.world.players[player_index]

    def find_player_by_index(self, player_index: int) -> Player:
        player = self.maybe_find_player_by_index(player_index)
        if not player:
            raise Exception(f"Player {player_index} not found.")
        return player

    def maybe_find_npc_by_index(self, npc_index: int) -> Optional[Npc]:
        if npc_index >= len(self.world.npcs):
            return None
        return self.world.npcs[npc_index]

    def find_npc_by_index(self, npc_index: int) -> Optional[Npc]:
        npc = self.maybe_find_npc_by_index(npc_index)
        if not npc:
            raise Exception(f"NPC {npc_index} not found.")
        return npc
