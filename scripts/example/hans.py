from potato_cactus import EventHandler, GameEvent, Context
from potato_cactus.api.actions import SpawnNpc, NpcSetForcedChat, ClearPlayerInteraction, InvokeScript, CreateInterface
from potato_cactus.api.dto.interface import ChatboxRootWindowElement, TextElement, NpcChatheadElement, \
    ModelAnimationElement
from potato_cactus.api.dto.position import Position
from potato_cactus.api.dto.script_invocation import ScriptInvocation
from potato_cactus.api.events import NpcEntityTickEventPayload, NpcInteractionEventPayload


@EventHandler(GameEvent.NpcEntityTickEvent, npcId=0)
def onNpcTick(e: NpcEntityTickEventPayload):
    return []


def invoked(playerIndex: int, arg1: str):
    return []


@EventHandler(GameEvent.NpcInteractionEvent, npcId=0)
def onNpcInteraction(e: NpcInteractionEventPayload, context: Context):
    player = context.find_player_by_index(e.playerIndex)
    return [
        CreateInterface(e.playerIndex, "standard", [
            NpcChatheadElement(4883, 0),
            ModelAnimationElement(4883, 591),
            TextElement(4884, "Hans"),
            TextElement(4885, "Hello world"),
            ChatboxRootWindowElement(4882)], onClose=ScriptInvocation(on_dialogue_screen, (e.playerIndex, 2))),
        ClearPlayerInteraction(e.playerIndex)]
    # return [NpcSetForcedChat(e.interaction.target.npcIndex, f"Hello {player.username}!"),
    #         ClearPlayerInteraction(e.playerIndex)]


def on_dialogue_screen(playerIndex: int, step: int):
    if step == 2:
        return [
            CreateInterface(playerIndex, "standard", [
                NpcChatheadElement(4883, 0),
                ModelAnimationElement(4883, 591),
                TextElement(4884, "Hans"),
                TextElement(4885, "Second screen!"),
                ChatboxRootWindowElement(4882)], onClose=ScriptInvocation(on_dialogue_screen, (playerIndex, 3)))]
    if step == 3:
        return [
            CreateInterface(playerIndex, "standard", [
                NpcChatheadElement(4883, 1),
                ModelAnimationElement(4883, 591),
                TextElement(4884, "some npc"),
                TextElement(4885, "Okay, this is the third screen"),
                ChatboxRootWindowElement(4882)])]


@EventHandler(GameEvent.ServerInitEvent)
def onServerInit(e):
    return [SpawnNpc(0, Position(3219, 3223, 0))]


def dialogue_example():
    root = DialogueBuilder("Hans", "surprised", "Hello!") \
        .addNode("Hans", "surprised", "This text is on the second screen.")

    return [ShowInterface()]
