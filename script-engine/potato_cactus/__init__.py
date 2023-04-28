from potato_cactus.api.context import Context
from potato_cactus.internal.impl.context_impl import ContextImpl
from potato_cactus.internal.registry import EventHandler
from potato_cactus.api.events import GameEvent


def get_context() -> Context:
    return ContextImpl.INSTANCE
