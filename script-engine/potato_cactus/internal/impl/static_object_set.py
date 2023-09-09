from typing import Dict, List, Optional, Tuple, NamedTuple

from potato_cactus.api.dto.object import GameObject, Position

TKey = Tuple[int, int, int, int]


class StaticObjectSet(object):

    def __init__(self):
        self._objects: Dict[TKey, _GameObject] = {}

    def load_objects(self, objects: List[GameObject]):
        for obj in objects:
            self._objects[_obj_key(obj)] = _GameObject(obj.id,
                                                       _Position(obj.position.x, obj.position.y, obj.position.z),
                                                       obj.objectType, obj.facingDirection)

    def find_object(self, position: Position,
                    objectType: int) -> Optional[GameObject]:
        return self._objects.get(_key(position, objectType))


def _obj_key(object: GameObject) -> TKey:
    return (
        object.position.x,
        object.position.y,
        object.position.z,
        object.objectType,
    )


def _key(position: Position, objectType: int) -> TKey:
    return (position.x, position.y, position.z, objectType)


class _Position(NamedTuple):
    x: int
    y: int
    z: int


class _GameObject(NamedTuple):
    id: int
    position: _Position
    objectType: int
    facingDirection: int
