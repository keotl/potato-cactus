import importlib
import pkgutil
from inspect import signature
from typing import List, Optional, Tuple, Union, cast

from potato_cactus.api.dto.world import World
from potato_cactus.api.events import GameEvent
from potato_cactus.internal.impl.context_impl import ContextImpl
from potato_cactus.internal.messages.inbound import InboundMessage
from potato_cactus.internal.messages.outbound import \
    internal_processingComplete
from potato_cactus.internal.registry import Registry

from ..util.stderr_logger import Logger
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
        elif message.op == "invokeScript":
            try:
                modulename, function = message.body.event.rsplit(".", 1)
                module = importlib.import_module(modulename)
                res = getattr(module, function)(*message.body.body)
                for action in res or []:
                    self._sender.send(action.__dict__)
            except KeyboardInterrupt as e:
                raise e
            except Exception as e:
                _logger.error(
                    f"Unhandled exception while invoking script '{message.body.event}'. {e}"
                )
        elif message.op == "gameEvent":
            _enrich_message(ContextImpl.INSTANCE, message.body)  # type: ignore
            handlers = Registry.INSTANCE.get_handlers(_event_key(message.body))
            if not handlers:
                default_handler_key = _default_event_handler_key(message.body)
                if default_handler_key:
                    handlers = Registry.INSTANCE.get_handlers(
                        default_handler_key)
            for h in handlers:
                try:
                    sig = signature(h)
                    if len(sig.parameters) == 2:
                        res = h(message.body.body, ContextImpl.INSTANCE)
                    elif len(sig.parameters) == 1:
                        res = h(message.body.body)
                    else:
                        res = h()
                    for action in res or []:
                        self._sender.send(action.__dict__)
                except KeyboardInterrupt as e:
                    raise e
                except Exception as e:
                    _logger.error(
                        f"Unhandled exception while invoking script {message.body.event}. {e}"
                    )


def _event_key(payload) -> Tuple[Optional[Union[str, int]], ...]:
    if payload.event == GameEvent.ServerInitEvent:
        return GameEvent.ServerInitEvent,
    if payload.event == GameEvent.ObjectInteractionEvent:
        return GameEvent.ObjectInteractionEvent, payload.body.interaction.target.objectId
    if payload.event == GameEvent.ItemOnObjectInteractionEvent:
        return GameEvent.ItemOnObjectInteractionEvent, payload.body.interaction.target.objectId
    if payload.event == GameEvent.NpcInteractionEvent:
        return GameEvent.NpcInteractionEvent, payload.body.interaction.target.npcId
    if payload.event == GameEvent.NpcAttackInteractionEvent:
        return GameEvent.NpcAttackInteractionEvent, payload.body.interaction.target.npcId
    if payload.event == GameEvent.PickupItemInteractionEvent:
        return GameEvent.PickupItemInteractionEvent, payload.body.interaction.target.itemId
    if payload.event == GameEvent.NpcAttackEvent:
        return GameEvent.NpcAttackEvent, payload.body.npcId
    if payload.event == GameEvent.PlayerAttackEvent:
        return GameEvent.PlayerAttackEvent,
    if payload.event == GameEvent.NpcDeadEvent:
        return GameEvent.NpcDeadEvent, payload.body.npcId
    if payload.event == GameEvent.NpcEntityTickEvent:
        return GameEvent.NpcEntityTickEvent, payload.body.npcId
    if payload.event == GameEvent.PlayerCommandEvent:
        return GameEvent.PlayerCommandEvent, payload.body.command
    if payload.event == GameEvent.DropItemEvent:
        return GameEvent.DropItemEvent, payload.body.itemId

    _logger.warning(
        f"Got event '{payload.event}' with an unconfigured key. No script will be invoked."
    )
    return "unassined",


def _default_event_handler_key(
        payload) -> Optional[Tuple[Optional[Union[str, int]], ...]]:
    if payload.event == GameEvent.ObjectInteractionEvent:
        return GameEvent.ObjectInteractionEvent, "default"
    if payload.event == GameEvent.ItemOnObjectInteractionEvent:
        return GameEvent.ItemOnObjectInteractionEvent, "default"
    if payload.event == GameEvent.NpcInteractionEvent:
        return GameEvent.NpcInteractionEvent, "default"
    if payload.event == GameEvent.NpcAttackInteractionEvent:
        return GameEvent.NpcAttackInteractionEvent, "default"
    if payload.event == GameEvent.PickupItemInteractionEvent:
        return GameEvent.PickupItemInteractionEvent, "default"
    if payload.event == GameEvent.NpcAttackEvent:
        return GameEvent.NpcAttackEvent, "default"
    if payload.event == GameEvent.NpcDeadEvent:
        return GameEvent.NpcDeadEvent, "default"
    if payload.event == GameEvent.NpcEntityTickEvent:
        return GameEvent.NpcEntityTickEvent, "default"
    if payload.event == GameEvent.PlayerCommandEvent:
        return GameEvent.PlayerCommandEvent, "default"
    if payload.event == GameEvent.DropItemEvent:
        return GameEvent.DropItemEvent, "default"

    return None


def _enrich_message(context: ContextImpl, payload):
    if payload.event == GameEvent.NpcInteractionEvent:
        # payload.body: NpcInteractionEventPayload
        payload.body.interaction.target["npcId"] = _find_npc_id(
            context, payload.body.interaction.target.npcIndex)
    if payload.event == GameEvent.NpcAttackInteractionEvent:
        # payload.body: NpcAttackInteractionEventPayload
        payload.body.interaction.target["npcId"] = _find_npc_id(
            context, payload.body.interaction.target.npcIndex)
    if payload.event == GameEvent.NpcAttackEvent:
        # payload.body: NpcAttackEventPayload
        payload.body["npcId"] = _find_npc_id(context, payload.body.npcIndex)
    if payload.event == GameEvent.NpcDeadEvent:
        # payload.body: NpcDeadEventPayload
        payload.body["npcId"] = _find_npc_id(context, payload.body.npcIndex)
    if payload.event == GameEvent.NpcEntityTickEvent:
        # payload.body: NpcEntityTickEventPayload
        payload.body["npcId"] = _find_npc_id(context, payload.body.npcIndex)


def _find_npc_id(context: ContextImpl, npc_index: int) -> int:
    npc = context.find_npc_by_index(npc_index)
    if npc:
        return npc.definitionId
    return 0


_logger = Logger("ScriptWorker")
