from dataclasses import dataclass
from .position import Position

@dataclass
class GameObject(object):
    id: int
    position: Position
    objectType: int
    facingDirection: int
