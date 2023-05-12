from potato_cactus import EventHandler, GameEvent, get_context
from potato_cactus.api.actions import ClearPlayerInteraction, SetPlayerPosition
from potato_cactus.api.events import ObjectInteractionEventPayload
from potato_cactus.helper.dialogue import (DialogueCallbackRef, DialogueNode,
                                           OptionsDialogueScreen,
                                           start_dialogue)


@EventHandler(GameEvent.ObjectInteractionEvent, objectId=1747)
def on_interact_ladder_up(e: ObjectInteractionEventPayload):
    return go_up(e.playerIndex)


@EventHandler(GameEvent.ObjectInteractionEvent, objectId=1746)
def on_interact_ladder_down(e: ObjectInteractionEventPayload):
    return go_down(e.playerIndex)


@EventHandler(GameEvent.ObjectInteractionEvent, objectId=1748)
def on_interact_ladder_up_down(e: ObjectInteractionEventPayload):
    if not e.interaction.target:
        pass
    elif e.interaction.target.actionIndex == 1:
        return start_dialogue(ladder_dialogue.ref, e.playerIndex)
    elif e.interaction.target.actionIndex == 2:
        return go_up(e.playerIndex)
    elif e.interaction.target.actionIndex == 3:
        return go_down(e.playerIndex)

    return [ClearPlayerInteraction(e.playerIndex)]


def go_up(playerIndex: int):
    player = get_context().find_player_by_index(playerIndex)
    if player is None:
        return []
    pos = player.movement.position
    return [
        SetPlayerPosition(playerIndex, (pos.x, pos.y, pos.z + 1)),
        ClearPlayerInteraction(playerIndex)
    ]


def go_down(playerIndex: int):
    player = get_context().find_player_by_index(playerIndex)
    if player is None:
        return []
    pos = player.movement.position
    return [
        SetPlayerPosition(playerIndex, (pos.x, pos.y, pos.z - 1)),
        ClearPlayerInteraction(playerIndex)
    ]
ladder_dialogue = DialogueNode(__name__, "ladder_dialogue") \
    .add(OptionsDialogueScreen([("Climb up", DialogueCallbackRef(go_up)),
                                ("Climb down", DialogueCallbackRef(go_down))]))
