from potato_cactus import Context, EventHandler, GameEvent
from potato_cactus.api.actions import SendMessage, SpawnNpc
from potato_cactus.api.events import PlayerCommandEventPayload


@EventHandler(GameEvent.PlayerCommandEvent, command="npc")
@EventHandler(GameEvent.PlayerCommandEvent, command="spawnNpc")
def on_command(e: PlayerCommandEventPayload, context: Context):
    player = context.find_player_by_index(e.playerIndex)
    if player is None:
        return []
    if len(e.args) < 1:
        return [SendMessage(e.playerIndex, f"Usage: ::{e.command} <npcId>")]
    return [SpawnNpc(int(e.args[0]), player.movement.position)]
