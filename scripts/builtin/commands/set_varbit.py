from potato_cactus import Context, EventHandler, GameEvent
from potato_cactus.api.actions import SendMessage, SetVarbit
from potato_cactus.api.events import PlayerCommandEventPayload


@EventHandler(GameEvent.PlayerCommandEvent, command="varbit")
def on_command(e: PlayerCommandEventPayload, context: Context):
    player = context.find_player_by_index(e.playerIndex)
    if player is None:
        return []
    if len(e.args) < 4:
        return [
            SendMessage(
                e.playerIndex,
                f"Usage: ::{e.command} <varpId> <lsb> <length> <value>")
        ]
    return [
        SetVarbit(e.playerIndex, int(e.args[0]), int(e.args[1]),
                  int(e.args[2]), int(e.args[3])),
        SendMessage(
            e.playerIndex,
            f"Set varbit varp={e.args[0]},lsb={e.args[1]},length={e.args[2]},value={e.args[4]}"
        ),
    ]
