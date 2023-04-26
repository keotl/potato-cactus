# Scripting API
PotatoCactus supports a Python scripting API. Scripts can be
registered to specific game **events**, and return **actions** to
apply modifications to the world instance. Any context available to
Python scripts is considered *read-only*.

## Events
Event names and payload types can be imported from `potato_cactus.api.events`.

| Event                  | Key                                                              | Description                                                         |
|------------------------|------------------------------------------------------------------|---------------------------------------------------------------------|
| ServerInitEvent        | N/A                                                              | Sent on server initialization. Use to spawn entities, objects, etc. |
| NpcEntityTickEvent     | `npcId`                                                          | Sent every tick for NPCs. Use for AI, movement, etc.                |
| PlayerInteractionEvent | `objectId` or `(npcId,type="npcAttack")` or `(npcId,type="npc")` | Sent on the tick when the player triggers the interaction.          |
| PlayerAttackEvent      |                                                                  |                                                                     |
| NpcAttackEvent         |                                                                  |                                                                     |
| NpcDeadEvent           |                                                                  |                                                                     |



## Actions
