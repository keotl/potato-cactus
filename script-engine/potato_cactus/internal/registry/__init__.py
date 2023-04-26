import functools
from collections import defaultdict
from typing import Tuple

from potato_cactus.api.events import GameEvent


def EventHandler(event: GameEvent, **options):  # TODO - Define options format  - keotl 2023-04-18
    def decorator(f):
        Registry.INSTANCE.register(event, options, f)

        @functools.wraps(f)
        def wrapper(*args, **kwargs):
            return f(*args, **kwargs)

        return wrapper

    return decorator


class Registry(object):
    INSTANCE: "Registry" = None  # type: ignore

    def __init__(self):
        self._content = defaultdict(lambda: [])

    def register(self, event: GameEvent, options, handler):
        self._content[event].append(handler)

    @staticmethod
    def key(event: GameEvent, options) -> Tuple[str]:
        if event == GameEvent.ServerInitEvent:
            return event,
        if event == GameEvent.NpcEntityTickEvent:
            return event, options.get("npcId")
        if event == GameEvent.PlayerInteractionEvent:
            return event, options.get("objectId")

Registry.INSTANCE = Registry()


