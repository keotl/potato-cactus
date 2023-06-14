from potato_cactus import Context, EventHandler, GameEvent
from potato_cactus.api.actions import SendMessage, SetPlayerAnimation
from potato_cactus.api.events import PlayerCommandEventPayload


@EventHandler(GameEvent.PlayerCommandEvent, command="animation")
@EventHandler(GameEvent.PlayerCommandEvent, command="anim")
def animation_command(e: PlayerCommandEventPayload, context: Context):
    player = context.find_player_by_index(e.playerIndex)
    if player is None:
        return []
    if len(e.args) < 1:
        return [SendMessage(e.playerIndex, f"Usage: ::{e.command} <animationId>")]
    return [SetPlayerAnimation(e.playerIndex, int(e.args[0]))]

