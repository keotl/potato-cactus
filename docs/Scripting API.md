# Scripting API
PotatoCactus supports a Python scripting API. Scripts can be
registered to specific game **events**, and return **actions** to
apply modifications to the world instance. Any context available to
Python scripts is considered *read-only*.

## Events
Event names and payload types can be imported from `potato_cactus.api.events`.

| Event                     | Key        | Description                                                         |
|---------------------------|------------|---------------------------------------------------------------------|
| ServerInitEvent           | N/A        | Sent on server initialization. Use to spawn entities, objects, etc. |
| NpcEntityTickEvent        | `npcId`    | Sent every tick for NPCs. Use for AI, movement, etc.                |
| ObjectInteractionEvent    | `objectId` | Sent on the tick when the player triggers the interaction.          |
| NpcInteractionEvent       | `npcId`    | Sent on the tick when the player triggers the interaction.          |
| NpcAttackInteractionEvent | `npcId`    | TODO is this still necessary?                                       |
| PlayerAttackEvent         |            |                                                                     |
| NpcAttackEvent            |            |                                                                     |
| NpcDeadEvent              |            |                                                                     |



## Actions
