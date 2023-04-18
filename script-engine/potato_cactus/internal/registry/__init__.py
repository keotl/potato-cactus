import functools
from collections import defaultdict
from potato_cactus.api.events import GameEvent

def EventHandler(event: GameEvent, options=None): # TODO - Define options format  - keotl 2023-04-18
    def decorator(f):
        Registry.INSTANCE.register(event, f)
        @functools.wraps(f)
        def wrapper(*args, **kwargs):
            return f(*args, **kwargs)
        return wrapper
    return decorator

class Registry(object):
    INSTANCE: "Registry" = None  # type: ignore

    def __init__(self):
        self._content = defaultdict(lambda : [])

    def register(self, event: GameEvent, handler):
        self._content[event].append(handler)

Registry.INSTANCE = Registry()
