import pkgutil
from typing import List, cast

import potato_cactus
from . import OutboundMessageSender, WorkerHandle
from potato_cactus.internal.messages.inbound import InboundMessage
from potato_cactus.internal.messages.outbound import internal_processingComplete
from potato_cactus.api.dto.world import World
from potato_cactus.internal.impl.context_impl import ContextImpl


class SimpleWorker(WorkerHandle):

    def __init__(self, sender: OutboundMessageSender, scriptPaths: List[str]):
        self._sender = sender
        self._discover_scripts(scriptPaths)
        ContextImpl.INSTANCE = ContextImpl()
        potato_cactus.context = ContextImpl.INSTANCE

    def _discover_scripts(self, paths: List[str]):
        for importer, modname, ispkg in pkgutil.walk_packages(paths):
            __import__(modname, fromlist="dummy")

    def dispatch(self, message: InboundMessage):
        if message.op == "doneSendingEvents":
            self._sender.send(internal_processingComplete())

        elif message.op == "updateWorld":
            world = cast(World, message.body)
            ContextImpl.INSTANCE.set_world(world)

        # elif message.op == "gameEvent":
        # TODO enrich payloads for npc references, e.g. interaction

