from potato_cactus import Context, EventHandler, GameEvent, get_context
from potato_cactus.api.actions import (GiveItem, SendMessage,
                                       SetPlayerEntityData, SpawnNpc,
                                       SubtractItem)
from potato_cactus.api.dto.player import Player
from potato_cactus.api.events import NpcInteractionEventPayload
from potato_cactus.helper.dialogue import (DialogueCallbackRef, DialogueNode,
                                           NpcDialogueScreen,
                                           PlayerDialogueScreen,
                                           start_dialogue)

NPC_ID = 278
BUCKET_OF_MILK = 1927
EGG = 1944
POT_OF_FLOUR = 1933
CAKE = 1895
COINS = 617


def start_quest(playerIndex: int):
    player = get_context().find_player_by_index(playerIndex)
    if player is None:
        return []

    return [SetPlayerEntityData(playerIndex, "cooks_assistant.started", True)]


def subtract_ingredients(playerIndex: int):
    return [
        SubtractItem(playerIndex, BUCKET_OF_MILK),
        SubtractItem(playerIndex, EGG),
        SubtractItem(playerIndex, POT_OF_FLOUR)
    ]


def complete_quest(playerIndex: int):
    return [
        SetPlayerEntityData(playerIndex, "cooks_assistant.completed", True),
        GiveItem(playerIndex, CAKE),
        GiveItem(playerIndex, COINS, 100)
    ]


dialogue_root = DialogueNode(__name__, "dialogue_root")
dialogue_root.add(NpcDialogueScreen(NPC_ID, "Cook", ["Oh no! What am I supposed to do?"], "worried")) \
    .add(PlayerDialogueScreen(["What's wrong?"])) \
    .add(NpcDialogueScreen(NPC_ID, "Cook", ["I need to bake a cake for the Duke's ", "birthday but I haven't got any ingredients!"])) \
    .add(PlayerDialogueScreen(["Maybe I could help just this one time..."], "skeptical")) \
    .add(NpcDialogueScreen(NPC_ID, "Cook", ["That you so much adventurer!",
                                            "Bring me a pot of flour, an egg and a bucket of milk",
                                            "and I shall reward you handsomely."], "default", onContinue=DialogueCallbackRef(start_quest)))

in_progress_dialogue = DialogueNode(__name__, "in_progress_dialogue")
in_progress_dialogue.add(
    NpcDialogueScreen(NPC_ID, "Cook", [
        "Have you got the ingredients I requested?",
        "Come back to me when you have a pot of flour",
        "an egg and a bucket of milk."
    ]))

completion_dialogue = DialogueNode(__name__, "completion_dialogue")
completion_dialogue.add(
    PlayerDialogueScreen(
        ["Here are the ingredients you requested."],
        onContinue=DialogueCallbackRef(subtract_ingredients))).add(
            NpcDialogueScreen(NPC_ID,
                              "Cook", [
                                  "Thank you so much adventurer!",
                                  "Please take this as a reward."
                              ],
                              onContinue=DialogueCallbackRef(complete_quest)))

post_completion_dialogue = DialogueNode(__name__, "post_completion_dialogue")
post_completion_dialogue.add(
    NpcDialogueScreen(NPC_ID, "Cook", [
        "Thank you for your help adventurer.",
        "One day you might also become a cook yourself!"
    ]))


@EventHandler(GameEvent.NpcInteractionEvent, npcId=NPC_ID)
def on_talk(e: NpcInteractionEventPayload, context: Context):
    player = context.find_player_by_index(e.playerIndex)
    if player is None:
        return []
    if player.entityData.get("cooks_assistant.completed"):
        return start_dialogue(post_completion_dialogue.ref, e.playerIndex)
    if not player.entityData.get("cooks_assistant.started"):
        return start_dialogue(dialogue_root.ref, e.playerIndex)

    if has_item(BUCKET_OF_MILK, player) and has_item(
            POT_OF_FLOUR, player) and has_item(EGG, player):
        return start_dialogue(completion_dialogue.ref, e.playerIndex)

    return start_dialogue(in_progress_dialogue.ref, e.playerIndex)


def has_item(item_id: int, player: Player) -> bool:
    return any(stack is not None and stack.itemId == item_id
               for stack in player.inventory)


@EventHandler(GameEvent.ServerInitEvent)
def onServerInit(e):
    return [SpawnNpc(NPC_ID, (3208, 3213, 0))]
