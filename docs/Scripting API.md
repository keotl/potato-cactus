# Scripting API
PotatoCactus supports a Python scripting API. Scripts can be
registered to specific game **events**, and return **actions** to
apply modifications to the world instance. Any context available to
Python scripts is considered *read-only*.

## Events
Event names and payload types can be imported from
`potato_cactus.api.events`.

| Event                        | Key        | Description                                                         |
|------------------------------|------------|---------------------------------------------------------------------|
| ServerInitEvent              | N/A        | Sent on server initialization. Use to spawn entities, objects, etc. |
| NpcEntityTickEvent           | `npcId`    | Sent every tick for NPCs. Use for AI, movement, etc.                |
| ObjectInteractionEvent       | `objectId` | Sent on the tick when the player triggers the interaction.          |
| ItemOnObjectInteractionEvent | `objectId` | When a player uses an item on a game object.                        |
| NpcInteractionEvent          | `npcId`    | Sent on the tick when the player triggers the interaction.          |
| NpcAttackInteractionEvent    | `npcId`    | TODO is this still necessary?                                       |
| PlayerAttackEvent            |            |                                                                     |
| PlayerCommandEvent           | `command`  | Sent when a client command is issued. (e.g. `::position`            |
| NpcAttackEvent               |            |                                                                     |
| NpcDeadEvent                 |            |                                                                     |


### Event Handlers
To register an event handler, annotate a function with `@EventHandler`
supplying the event name and the required key attribute.

```python
from potato_cactus import EventHandler, GameEvent
from potato_cactus.api.events import NpcInteractionEventPayload

@EventHandler(GameEvent.NpcInteractionEvent, npcId=0)
def onNpcInteraction(e: NpcInteractionEventPayload):
	return [] # Return actions here
```

## Actions
Action constructors are imported from `potato_cactus.api.actions`.
| Action                 | Description                                                                                                              |
|------------------------|--------------------------------------------------------------------------------------------------------------------------|
| ClearPlayerInteraction | Marks the interaction as complete to prevent further `inProgress` events from being raised.                              |
| NpcQueueWalk           | Queues an NPC movement to a target position. The pathfinding calculation is done by the engine.                          |
| NpcSetAnimation        | Sets the NPC animation.                                                                                                  |
| NpcSetForcedChat       | Sets a forced chat message for an NPC.                                                                                   |
| SpawnGameObject        | Adds a dynamic game object to the world, overriding an existing object of the same type at that position.                |
| RemoveGameObject       | Adds a "removed" dynamic game object, which can be used to subtract an object from the static object set.                |
| ServerPrintMessage     | Prints to the server console. For testing.                                                                               |
| SpawnNpc               | Spawns an NPC at a point.                                                                                                |
| SendMessage            | Sends a server message to the player's chatbox                                                                           |
| CreateInterface        | Configure a player interface using low-level primitives. See `potato_cactus.api.dto.interface` for supported primitives. |
| ClearStandardInterface | Clear the primary interfaces, as though the player issued an action.                                                     |
| SetPlayerEntityData    | Write a key/value pair to the player data store.                                                                         |

