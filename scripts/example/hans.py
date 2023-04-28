import potato_cactus
from potato_cactus import EventHandler, GameEvent
from potato_cactus.api.actions import SpawnNpc, NpcSetForcedChat, ClearPlayerInteraction
from potato_cactus.api.dto.position import Position
from potato_cactus.api.events import NpcEntityTickEventPayload, NpcInteractionEventPayload


@EventHandler(GameEvent.NpcEntityTickEvent, npcId=0)
def onNpcTick(e: NpcEntityTickEventPayload):
    return []


@EventHandler(GameEvent.NpcInteractionEvent, npcId=0)
def onNpcInteraction(e: NpcInteractionEventPayload):
    try:
        context = potato_cactus.get_context()
        player = next(filter(lambda p: p.serverIndex == e.playerIndex, context.world.players))
        return [NpcSetForcedChat(e.interaction.target.npcIndex, f"Hello {player.username}!"),
                ClearPlayerInteraction(e.playerIndex)]
    except StopIteration:
        return []


@EventHandler(GameEvent.ServerInitEvent)
def onServerInit(e):
    return [SpawnNpc(0, Position(3093, 3250, 0))]
