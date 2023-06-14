import functools
from collections import defaultdict
from typing import Optional, Tuple, Union

from potato_cactus.api.events import GameEvent


def EventHandler(
        event: GameEvent,
        **options):  # TODO - Define options format  - keotl 2023-04-18

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

    def register(self, event: GameEvent, options: dict, handler):
        self._content[self.key(event, options)].append(handler)

    def get_handlers(self, key_elems: Tuple[Optional[Union[str, int]], ...]):
        return self._content.get(key_elems) or []

    @staticmethod
    def key(event: GameEvent,
            options: dict) -> Tuple[Optional[Union[str, int]], ...]:
        if event == GameEvent.ServerInitEvent:
            return event,
        elif event == GameEvent.NpcEntityTickEvent:
            return event, options.get("npcId")
        elif event == GameEvent.ObjectInteractionEvent:
            return event, options.get("objectId")
        elif event == GameEvent.ItemOnObjectInteractionEvent:
            return event, options.get("objectId")
        elif event == GameEvent.NpcInteractionEvent:
            return event, options.get("npcId")
        elif event == GameEvent.NpcAttackInteractionEvent:
            return event, options.get("npcId")
        elif event == GameEvent.PickupItemInteractionEvent:
            return event, options.get("itemId")
        elif event == GameEvent.PlayerAttackEvent:
            return event,
        elif event == GameEvent.NpcAttackEvent:
            return event, options.get("npcId")
        elif event == GameEvent.NpcDeadEvent:
            return event, options.get("npcId")
        elif event == GameEvent.PlayerCommandEvent:
            return event, options.get("command")
        elif event == GameEvent.DropItemEvent:
            return event, options.get("itemId")
        return "unassigned",


Registry.INSTANCE = Registry()
