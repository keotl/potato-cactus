from potato_cactus import EventHandler, GameEvent, Context
from potato_cactus.api.actions import SpawnNpc, NpcSetForcedChat, ClearPlayerInteraction
from potato_cactus.api.dto.position import Position
from potato_cactus.api.events import NpcEntityTickEventPayload, NpcInteractionEventPayload


@EventHandler(GameEvent.NpcEntityTickEvent, npcId=0)
def onNpcTick(e: NpcEntityTickEventPayload):
    return []


@EventHandler(GameEvent.NpcInteractionEvent, npcId=0)
def onNpcInteraction(e: NpcInteractionEventPayload, context: Context):
    player = context.find_player_by_index(e.playerIndex)
    return [NpcSetForcedChat(e.interaction.target.npcIndex, f"Hello {player.username}!"),
            ClearPlayerInteraction(e.playerIndex)]


@EventHandler(GameEvent.ServerInitEvent)
def onServerInit(e):
    return [SpawnNpc(0, Position(3093, 3250, 0))]
