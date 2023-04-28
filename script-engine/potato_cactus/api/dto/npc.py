from potato_cactus.api.dto.combat import Combat
from potato_cactus.api.dto.movement import Movement


class Npc(object):
    serverIndex: int
    definitionId: int
    movement: Movement
    combat: Combat
