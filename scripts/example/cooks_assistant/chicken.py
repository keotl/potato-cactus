from example.cooks_assistant.dairy_cow import SendMessage
from potato_cactus import Context, EventHandler, GameEvent
from potato_cactus.api.actions import NpcQueueWalk, SpawnGroundItem, SpawnNpc
from potato_cactus.api.dto.position import Position
from potato_cactus.api.events import (NpcDeadEventPayload,
                                      NpcEntityTickEventPayload)
from potato_cactus.helper.dialogue import random

BONES = 526
FEATHER = 314
EGG = 1944
CHICKEN = 41
WANDER_AREA = [Position(3185, 3276, 0), Position(3191, 3278, 0)]


@EventHandler(GameEvent.ServerInitEvent)
def spawn_chicken():
    return [SpawnNpc(CHICKEN, (3190, 3277, 0), 100)]


@EventHandler(GameEvent.NpcEntityTickEvent, npcId=CHICKEN)
def chicken_wander(e: NpcEntityTickEventPayload, context: Context):
    npc = context.find_npc_by_index(e.npcIndex)
    if npc is None:
        return []

    if npc.combat.target is None and _should_wander():
        return [NpcQueueWalk(e.npcIndex, _within_wander_radius())]

    return []


@EventHandler(GameEvent.NpcDeadEvent, npcId=CHICKEN)
def on_death(e: NpcDeadEventPayload, context: Context):
    npc = context.find_npc_by_index(e.npcIndex)
    if npc is None:
        return []

    return [
        # TODO - Set NPC animation  - keotl 2023-06-12
        SpawnGroundItem(EGG, 1, npc.movement.position),
        SpawnGroundItem(BONES, 1, npc.movement.position),
        SpawnGroundItem(FEATHER, 5, npc.movement.position),
    ]


def _should_wander():
    return random.random() > 0.9


def _within_wander_radius():
    return (random.randint(WANDER_AREA[0].x, WANDER_AREA[1].x),
            random.randint(WANDER_AREA[0].y,
                           WANDER_AREA[1].y), WANDER_AREA[0].z)
