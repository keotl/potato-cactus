from potato_cactus import EventHandler, GameEvent
from potato_cactus.api.actions import GiveItem, SpawnNpc
from potato_cactus.api.events import NpcInteractionEventPayload
from potato_cactus.helper.dialogue import (DialogueCallbackRef, DialogueNode,
                                           NpcDialogueScreen,
                                           PlayerDialogueScreen,
                                           start_dialogue)

NPC_ID = 531


def give_empty_pot(playerIndex: int):
    return [GiveItem(playerIndex, 1931)]


dialogue_root = DialogueNode(__name__, "dialogue_root")
dialogue_root.add(NpcDialogueScreen(NPC_ID, "Miller", ["Welcome to the mill."])) \
    .add(PlayerDialogueScreen(["Do you have any empty pots lying around?"])) \
    .add(NpcDialogueScreen(NPC_ID, "Miller", ["Of course. Here you go."],
                           onContinue=DialogueCallbackRef(give_empty_pot)))


@EventHandler(GameEvent.NpcInteractionEvent, npcId=NPC_ID)
def on_talk(e: NpcInteractionEventPayload):
    return start_dialogue(dialogue_root.ref, e.playerIndex)


@EventHandler(GameEvent.ServerInitEvent)
def onServerInit(e):
    return [SpawnNpc(NPC_ID, (3168, 3305, 0))]
