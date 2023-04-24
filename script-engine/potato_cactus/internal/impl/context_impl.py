from potato_cactus.api.context import Context
from potato_cactus.api.dto.world import World

class ContextImpl(Context):
    INSTANCE: "ContextImpl" = None  # type: ignore

    def __init__(self):
        if self.INSTANCE is not None:
            raise Exception("Context already initialized")
        self._world = None
        ContextImpl.INSTANCE = self

    @property
    def world(self):
        return self._world

    def set_world(self, world : World):
        self._world = world
