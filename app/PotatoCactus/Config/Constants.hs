module PotatoCactus.Config.Constants where

tickInterval :: Int
tickInterval = 600 * 1000

entityViewingDistance :: Int
entityViewingDistance = 15

groundItemGlobalDespawnDelay :: Int
groundItemGlobalDespawnDelay = 100

maxPlayers :: Int
maxPlayers = 2000

maxNpcs :: Int
maxNpcs = 16000

npcDisengageDistance :: Int
npcDisengageDistance = 20

scriptWorkers :: Int
scriptWorkers = 1

objectDefinitionsFile = "game-data/definitions/objects.json"
itemDefinitionsFile = "game-data/definitions/items.json"
mapFilesDirectory = "game-data/maps/"
mapObjectPlacementFileSuffix = ".objects.json"

-- Hashing function config
positionBoundExponentXY_ = 6 -- X or Y will be less than 10^6
positionBoundExponentZ_ = 1 -- Z will be less than 10^1
chunkBoundExponentXY_ = 5
