from abc import ABC
from potato_cactus.api.dto.world import World

class Context(ABC):
    world: World
