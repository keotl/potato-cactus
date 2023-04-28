import pkgutil
from typing import List, cast, Tuple, Optional, Union

from potato_cactus.api.dto.world import World
from potato_cactus.api.events import GameEvent
from potato_cactus.internal.impl.context_impl import ContextImpl
from potato_cactus.internal.messages.inbound import InboundMessage
from potato_cactus.internal.messages.outbound import internal_processingComplete
from potato_cactus.internal.registry import Registry
from . import OutboundMessageSender, WorkerHandle


class SimpleWorker(WorkerHandle):

    def __init__(self, sender: OutboundMessageSender, scriptPaths: List[str]):
        self._sender = sender
        self._discover_scripts(scriptPaths)
        ContextImpl.INSTANCE = ContextImpl()

    def _discover_scripts(self, paths: List[str]):
        for importer, modname, ispkg in pkgutil.walk_packages(paths):
            __import__(modname, fromlist="dummy")

    def dispatch(self, message: InboundMessage):
        if message.op == "doneSendingEvents":
            self._sender.send(internal_processingComplete())

        elif message.op == "updateWorld":
            world = cast(World, message.body)
            ContextImpl.INSTANCE.set_world(world)

        elif message.op == "gameEvent":
            _enrich_message(ContextImpl.INSTANCE.world, message.body)  # type: ignore
            handlers = Registry.INSTANCE.get_handlers(_event_key(message.body))
            for h in handlers:
                res = h(message.body.body)
                for action in res:
                    self._sender.send(action.__dict__)


def _event_key(payload) -> Tuple[Optional[Union[str, int]], ...]:
    if payload.event == GameEvent.ServerInitEvent:
        return GameEvent.ServerInitEvent,
    if payload.event == GameEvent.ObjectInteractionEvent:
        return GameEvent.ObjectInteractionEvent, payload.body.interaction.target.objectId
    if payload.event == GameEvent.NpcInteractionEvent:
        return GameEvent.NpcInteractionEvent, payload.body.interaction.target.npcId
    if payload.event == GameEvent.NpcAttackInteractionEvent:
        return GameEvent.NpcAttackInteractionEvent, payload.body.interaction.target.npcId
    if payload.event == GameEvent.NpcAttackEvent:
        return GameEvent.NpcAttackEvent, payload.body.npcId
    if payload.event == GameEvent.PlayerAttackEvent:
        return GameEvent.PlayerAttackEvent,
    if payload.event == GameEvent.NpcDeadEvent:
        return GameEvent.NpcDeadEvent, payload.body.npcId
    if payload.event == GameEvent.NpcEntityTickEvent:
        return GameEvent.NpcEntityTickEvent, payload.body.npcId

    return "unassined",


def _enrich_message(world: World, payload):
    if payload.event == GameEvent.NpcInteractionEvent:
        # payload.body: NpcInteractionEventPayload
        payload.body.interaction.target["npcId"] = _find_npc_id(world, payload.body.interaction.target.npcIndex)
    if payload.event == GameEvent.NpcAttackInteractionEvent:
        # payload.body: NpcAttackInteractionEventPayload
        payload.body.interaction.target["npcId"] = _find_npc_id(world, payload.body.interaction.target.npcIndex)
    if payload.event == GameEvent.NpcAttackEvent:
        # payload.body: NpcAttackEventPayload
        payload.body["npcId"] = _find_npc_id(world, payload.body.npcIndex)
    if payload.event == GameEvent.NpcDeadEvent:
        # payload.body: NpcDeadEventPayload
        payload.body["npcId"] = _find_npc_id(world, payload.body.npcIndex)
    if payload.event == GameEvent.NpcEntityTickEvent:
        # payload.body: NpcEntityTickEventPayload
        payload.body["npcId"] = _find_npc_id(world, payload.body.npcIndex)


def _find_npc_id(world: World, npcIndex: int) -> int:
    # TODO - Make sure we can do this in O(1) time  - keotl 2023-04-27
    try:
        return next(filter(lambda n: n.serverIndex == npcIndex, world.npcs)).definitionId
    except StopIteration:
        return 0
