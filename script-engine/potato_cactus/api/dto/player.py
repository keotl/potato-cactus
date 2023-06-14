from typing import List, Optional

from potato_cactus.api.dto.combat import Combat
from potato_cactus.api.dto.interaction import PlayerInteraction
from potato_cactus.api.dto.inventory import ItemStack
from potato_cactus.api.dto.movement import Movement


class Player(object):
    serverIndex: int
    username: str
    movement: Movement
    combat: Combat
    interaction: PlayerInteraction
    inventory: List[Optional[ItemStack]]
    equipment: List[Optional[ItemStack]]
    entityData: dict
