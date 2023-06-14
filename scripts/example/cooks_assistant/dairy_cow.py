from example.windmill.miller import GameEvent
from potato_cactus import Context, EventHandler
from potato_cactus.api.actions import (ClearPlayerInteraction, GiveItem,
                                       InvokeScript, SendMessage,
                                       SetPlayerAnimation, SpawnGroundItem,
                                       SubtractItem)
from potato_cactus.api.dto.player import Player
from potato_cactus.api.events import (ItemOnObjectInteractionEventPayload,
                                      ObjectInteractionEventPayload)

DAIRY_COW = 8689
EMPTY_BUCKET = 1925
BUCKET_OF_MILK = 1927
ANIMATION_ID = 645


@EventHandler(GameEvent.ItemOnObjectInteractionEvent, objectId=DAIRY_COW)
def on_use_item_interaction(e: ItemOnObjectInteractionEventPayload,
                            context: Context):
    player = context.find_player_by_index(e.playerIndex)
    if player is None or e.interaction.target is None:
        return []

    if e.interaction.target.itemId != EMPTY_BUCKET:
        return [
            SendMessage(e.playerIndex, "Nothing interesting happens."),
            ClearPlayerInteraction(e.playerIndex)
        ]

    return _do_interaction(player)


@EventHandler(GameEvent.ObjectInteractionEvent, objectId=DAIRY_COW)
def on_object_interaction(e: ObjectInteractionEventPayload, context: Context):
    player = context.find_player_by_index(e.playerIndex)
    if player is None or e.interaction.target is None:
        return []
    return _do_interaction(player)


def _do_interaction(player: Player):
    if not _has_empty_bucket(player):
        return [
            ClearPlayerInteraction(player.serverIndex),
            SetPlayerAnimation(player.serverIndex, ANIMATION_ID),
            SendMessage(player.serverIndex,
                        "You need an empty bucket to gather the milk.")
        ]

    return [
        SubtractItem(player.serverIndex, EMPTY_BUCKET, 1),
        GiveItem(player.serverIndex, BUCKET_OF_MILK, 1),
        SendMessage(player.serverIndex, "You gather milk in the bucket."),
        ClearPlayerInteraction(player.serverIndex)
    ]


def _has_empty_bucket(player: Player) -> bool:
    return any(stack is not None and stack.itemId == EMPTY_BUCKET
               for stack in player.inventory)


@EventHandler(GameEvent.ServerInitEvent)
def bucket_spawn():
    return [
        SpawnGroundItem(EMPTY_BUCKET, 1, (3168, 3319, 0)),
        InvokeScript(bucket_spawn, delay=100)
    ]
