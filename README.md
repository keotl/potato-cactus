# potato-cactus
![A potato
cactus](https://oldschool.runescape.wiki/images/thumb/Potato_cactus_detail.png/130px-Potato_cactus_detail.png?1bf07)

A Runescape server engine written in Haskell. Targets revision 317. 

Takes some inspiration from
[luna-rs](https://github.com/luna-rs/luna) and [317refactor](https://github.com/Jameskmonger/317refactor).

## Engine demo (v0.0.1, 2023-06-14)
[![2023-06-14](https://img.youtube.com/vi/q1qQ_Inp_QI/0.jpg)](https://www.youtube.com/watch?v=q1qQ_Inp_QI)


## What works
- [x] Players can login
- [x] Players can walk around the world and load the map around them
- [x] Players can chat to each other
- [x] Players can equip armour and weapons
- [x] Players can seen and interact with NPCs
- [x] Players can seen and interact with game objects
- [x] The world can be configured using Python scripts
- [x] Players can drop and pick up items
- [x] Dialogue interfaces can be invoked and trigger callbacks
- [x] Scripts can be invoked dynamically through timeouts or callbacks

## Priorities
- [ ] Instanced game objects
- [ ] Game data loading
  - [ ] Item, NPC, Object definitions
  - [ ] Collision map and pathing
  - [ ] Static object set
- [ ] Persistence mechanism

## Running requirements
- Revision-317 cache files
- Revision-317 client with encryption disabled

## Debugging
### Running with stacktraces
```
cabal configure --enable-profiling --profiling-detail toplevel-functions
cabal run potato-cactus -- +RTS -xc
```
