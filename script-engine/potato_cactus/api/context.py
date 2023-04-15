from abc import ABC
from potato_cactus.api.world import World

class Context(ABC):
    world: World
