from potato_cactus import Context, EventHandler, GameEvent, get_context
from potato_cactus.api.actions import CreateInterface, SpawnNpc
from potato_cactus.api.dto.interface import (ChatboxRootWindowElement,
                                             ModelAnimationElement,
                                             NpcChatheadElement,
                                             PlayerChatheadElement,
                                             TextElement)
from potato_cactus.api.dto.position import Position
from potato_cactus.api.dto.script_invocation import ScriptInvocation
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


def on_dialogue_screen(playerIndex: int, step: int):
    player = get_context().find_player_by_index(playerIndex)
    if player is None:
        return []
    if step == 2:
        return [
            CreateInterface(playerIndex,
                            "standard", [
                                PlayerChatheadElement(969),
                                ModelAnimationElement(969, 591),
                                TextElement(970, player.username),
                                TextElement(971, "Second screen!"),
                                ChatboxRootWindowElement(968)
                            ],
                            onClose=ScriptInvocation(on_dialogue_screen,
                                                     (playerIndex, 3)))
        ]
    if step == 3:
        return [
            CreateInterface(playerIndex, "standard", [
                NpcChatheadElement(4883, 1),
                ModelAnimationElement(4883, 591),
                TextElement(4884, "some npc"),
                TextElement(4885, "Okay, this is the third screen"),
                ChatboxRootWindowElement(4882)
            ])
        ]


@EventHandler(GameEvent.ServerInitEvent)
def onServerInit(e):
    return [SpawnNpc(0, Position(3219, 3223, 0))]
