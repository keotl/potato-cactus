from typing import List

from potato_cactus.api.dto.position import Position


class Movement(object):
    position: Position
    queue: List[Position]
    isRunning: bool
