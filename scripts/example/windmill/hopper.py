from potato_cactus import Context, EventHandler, GameEvent
from potato_cactus.api.actions import (ClearPlayerInteraction, GiveItem,
                                       SendMessage, SetPlayerAnimation,
                                       SetPlayerEntityData, SetVarbit,
                                       SubtractItem)
from potato_cactus.api.dto.player import Player
from potato_cactus.api.events import (ItemOnObjectInteractionEventPayload,
                                      ObjectInteractionEventPayload)


@EventHandler(GameEvent.ItemOnObjectInteractionEvent, objectId=2714)
def on_put_item(e: ItemOnObjectInteractionEventPayload, context: Context):
    player = context.find_player_by_index(e.playerIndex)

    if player is None:
        return []

    current_grain = player.entityData.get("windmill.grain", 0)

    return [
        SubtractItem(e.playerIndex, 1947, 1),
        SetPlayerEntityData(e.playerIndex, "windmill.grain",
                            current_grain + 1),
        SendMessage(e.playerIndex, f"Current grain: {current_grain + 1}"),
        SetPlayerAnimation(e.playerIndex, 832),
        ClearPlayerInteraction(e.playerIndex)
    ]


@EventHandler(GameEvent.ObjectInteractionEvent, objectId=2718)
def on_interact_controls(e: ObjectInteractionEventPayload, context: Context):
    player = context.find_player_by_index(e.playerIndex)
    if player is None or e.interaction.target is None:
        return []

    current_grain = player.entityData.get("windmill.grain", 0)
    current_flour = player.entityData.get("windmill.flour", 0)
    if current_grain + current_flour > 0:
        return [
            SetPlayerEntityData(e.playerIndex, "windmill.grain", 0),
            SetPlayerEntityData(e.playerIndex, "windmill.flour",
                                current_grain + current_flour),
            SetVarbit(player.serverIndex, 203, 0, 4, 1),
            SendMessage(e.playerIndex,
                        f"Current flour: {current_grain + current_flour}"),
            SetPlayerAnimation(e.playerIndex, 832),
            ClearPlayerInteraction(e.playerIndex)
        ]
    return [
        ClearPlayerInteraction(e.playerIndex),
        SetPlayerAnimation(e.playerIndex, 832)
    ]


@EventHandler(GameEvent.ObjectInteractionEvent, objectId=1781)
def on_interact_flour_bin(e: ObjectInteractionEventPayload, context: Context):
    player = context.find_player_by_index(e.playerIndex)
    if player is None or e.interaction.target is None:
        return []

    current_flour = player.entityData.get("windmill.flour", 0)
    if current_flour == 0:
        return [ClearPlayerInteraction(e.playerIndex)]
    if not _has_empty_pot(player):
        return [
            ClearPlayerInteraction(e.playerIndex),
            SendMessage(e.playerIndex,
                        "You need an empty pot to hold the flour in.")
        ]

    return [
        SetPlayerEntityData(e.playerIndex, "windmill.flour",
                            current_flour - 1),
        SubtractItem(e.playerIndex, 1931),
        GiveItem(e.playerIndex, 1933),
        SendMessage(e.playerIndex, f"Current flour: {current_flour - 1}"),
        SetPlayerAnimation(e.playerIndex, 832)
    ] + ([
        SetVarbit(player.serverIndex, 203, 0, 4, 0),
    ] if current_flour == 1 else [])


def _has_empty_pot(player: Player) -> bool:
    return any(stack is not None and stack.itemId == 1931
               for stack in player.inventory)
