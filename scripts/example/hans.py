from potato_cactus import Context, EventHandler, GameEvent
from potato_cactus.api.actions import SpawnNpc
from potato_cactus.api.dto.position import Position
from potato_cactus.api.events import NpcInteractionEventPayload
from potato_cactus.helper.dialogue import (DialogueNode, NpcDialogueScreen,
                                           OptionsDialogueScreen,
                                           PlayerDialogueScreen,
                                           start_dialogue)

intimidation_dialogue_node = DialogueNode(
    __name__, "intimidation_dialogue_node", [
        PlayerDialogueScreen(["I'm here to kill everyone in this castle!"],
                             "angry_laughing"),
        NpcDialogueScreen(0, "Hans", ["Oh no!"], "worried")
    ])

dialogue_root = DialogueNode(__name__, "dialogue_root")
dialogue_root.add(NpcDialogueScreen(0, "Hans", ["Hello there!"])) \
    .add(PlayerDialogueScreen(["Hello!"])) \
    .add(NpcDialogueScreen(0, "Hans", ["What can I do for you?"])) \
    .add(OptionsDialogueScreen([
    ("I'm here to kill everyone in this castle!", intimidation_dialogue_node.ref),
    ("Could you say that again?", dialogue_root.ref)]))


@EventHandler(GameEvent.NpcInteractionEvent, npcId=0)
def onNpcInteraction(e: NpcInteractionEventPayload, context: Context):
    return start_dialogue(dialogue_root.ref, e.playerIndex)


@EventHandler(GameEvent.ServerInitEvent)
def onServerInit(e):
    return [SpawnNpc(0, Position(3219, 3223, 0))]
