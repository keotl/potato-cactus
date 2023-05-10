from potato_cactus import Context, EventHandler, GameEvent, get_context
from potato_cactus.api.actions import (ClearPlayerInteraction, CreateInterface,
                                       InvokeScript, NpcSetForcedChat,
                                       SpawnNpc)
from potato_cactus.api.dto.interface import (ChatboxRootWindowElement,
                                             ModelAnimationElement,
                                             NpcChatheadElement,
                                             PlayerChatheadElement,
                                             TextElement)
from potato_cactus.api.dto.position import Position
from potato_cactus.api.dto.script_invocation import ScriptInvocation
from potato_cactus.api.events import (NpcEntityTickEventPayload,
                                      NpcInteractionEventPayload)
from potato_cactus.helper.dialogue import (DialogueNode, NpcDialogueScreen,
                                           OptionsDialogueScreen,
                                           PlayerDialogueScreen)


@EventHandler(GameEvent.NpcEntityTickEvent, npcId=0)
def onNpcTick(e: NpcEntityTickEventPayload):
    return []


def invoked(playerIndex: int, arg1: str):
    return []


intimidation_dialogue_node = DialogueNode([
    PlayerDialogueScreen(["I'm here to kill everyone in this castle!"],
                         "angry_laughing"),
    NpcDialogueScreen(0, "Hans", ["Oh no!"], "worried")
])

dialogue_root = DialogueNode([
    NpcDialogueScreen(0, "Hans", ["Hello there!"]),
    PlayerDialogueScreen(["Hello, what can you do for me?"]),
    OptionsDialogueScreen([("I'm here to kill everyone in this castle!",
                            intimidation_dialogue_node),
                           ("repeat", dialogue_root)])
])


@EventHandler(GameEvent.NpcInteractionEvent, npcId=0)
def onNpcInteraction(e: NpcInteractionEventPayload, context: Context):
    player = context.find_player_by_index(e.playerIndex)
    return [
        CreateInterface(e.playerIndex,
                        "standard", [
                            NpcChatheadElement(4883, 0),
                            ModelAnimationElement(4883, 591),
                            TextElement(4884, "Hans"),
                            TextElement(4885, "Hello world"),
                            ChatboxRootWindowElement(4882)
                        ],
                        onClose=ScriptInvocation(on_dialogue_screen,
                                                 (e.playerIndex, 2)),
                        callbacks={
                            1234:
                                ScriptInvocation(on_dialogue_screen,
                                                 (e.playerIndex, 666))
                        }),
        ClearPlayerInteraction(e.playerIndex)
    ]
    # return [NpcSetForcedChat(e.interaction.target.npcIndex, f"Hello {player.username}!"),
    #         ClearPlayerInteraction(e.playerIndex)]


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


def dialogue_example():
    root = DialogueBuilder("Hans", "surprised", "Hello!") \
        .addNode("Hans", "surprised", "This text is on the second screen.")

    return [ShowInterface()]
