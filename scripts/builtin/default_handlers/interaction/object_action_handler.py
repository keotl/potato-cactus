from potato_cactus import EventHandler, GameEvent
from potato_cactus.api.actions import ClearPlayerInteraction, SendMessage
from potato_cactus.api.events import (ItemOnObjectInteractionEventPayload,
                                      ObjectInteractionEventPayload)


@EventHandler(GameEvent.ItemOnObjectInteractionEvent, objectId="unhandled")
def on_unhandled_item_on_object(e: ItemOnObjectInteractionEventPayload):

    return [
        SendMessage(
            e.playerIndex,
            f"Unhandled ItemOnObjectInteractionEvent for objectId={e.interaction.target.object.id}."
        ),
        SendMessage(e.playerIndex, "Nothing interesting happens."),
    ]


@EventHandler(GameEvent.ObjectInteractionEvent, objectId="unhandled")
def on_unhandled_object_action(e: ObjectInteractionEventPayload):

    return [
        SendMessage(
            e.playerIndex,
            f"Unhandled ObjectInteractionEvent for objectId={e.interaction.target.object.id}."
        ),
        SendMessage(e.playerIndex, "Nothing interesting happens."),
    ]


@EventHandler(GameEvent.ItemOnObjectInteractionEvent, objectId="default")
def on_item_on_object_default(e: ItemOnObjectInteractionEventPayload):

    return [ClearPlayerInteraction(e.playerIndex)]


@EventHandler(GameEvent.ObjectInteractionEvent, objectId="default")
def on_object_action_default(e: ObjectInteractionEventPayload):

    return [ClearPlayerInteraction(e.playerIndex)]
