from potato_cactus.api.context import Context
from potato_cactus.api.world import World

class ContextImpl(Context):

    def __init__(self):
        self._world = None

    @property
    def world(self):
        return self._world

    def set_world(self, world : World):
        self._world = world
