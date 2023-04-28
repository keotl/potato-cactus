# potato-cactus
![A potato
cactus](https://oldschool.runescape.wiki/images/thumb/Potato_cactus_detail.png/130px-Potato_cactus_detail.png?1bf07)

A Runescape server engine written in Haskell. Targets revision 317. 

Takes some inspiration from
[luna-rs](https://github.com/luna-rs/luna) and [317refactor](https://github.com/Jameskmonger/317refactor).

## What works
- [x] Players can login
- [x] Players can walk around the world and load the map around them
- [x] Players can chat to each other
- [x] Players can equip armour and weapons
- [x] Players can seen and interact with NPCs
- [x] Players can seen and interact with game objects
- [-] The world can be configured using Python scripts

## Priorities
- [ ] Augment scripting API actions and events
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
