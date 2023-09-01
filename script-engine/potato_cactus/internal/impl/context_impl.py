from typing import List, Optional

from potato_cactus.api.context import Context
from potato_cactus.api.dto.object import GameObject
from potato_cactus.api.dto.position import Position
from potato_cactus.api.dto.world import World
from potato_cactus.internal.impl.static_object_set import StaticObjectSet


class ContextImpl(Context):
    INSTANCE: "ContextImpl" = None  # type: ignore

    def __init__(self):
        if self.INSTANCE is not None:
            raise Exception("Context already initialized")
        self._world = None
        self._static_objects = StaticObjectSet()
        ContextImpl.INSTANCE = self

    @property
    def world(self):
        return self._world

    def set_world(self, world: World):
        self._world = world

    def load_static_objects(self, objects: List[GameObject]):
        self._static_objects.load_objects(objects)

    def find_static_game_object(self, position: Position,
                                objType: int) -> Optional[GameObject]:
        return self._static_objects.find_object(position, objType)
