module PotatoCactus.Client.LocalPlayerList (LocalPlayerList, LocalPlayerStatus (Added, Removed, Retained), LocalPlayer (LocalPlayer), updateLocalPlayers) where

import Data.List (find)
import Data.Maybe (catMaybes, isNothing, mapMaybe)
import PotatoCactus.Config.Constants (entityViewingDistance)
import PotatoCactus.Game.Player (Player (username))
import PotatoCactus.Game.Position (GetPosition (getPosition), Position, isWithin)
import PotatoCactus.Game.Typing (Advance (advance))

data LocalPlayerStatus = Added | Removed | Retained deriving (Show)

data LocalPlayer = LocalPlayer Player LocalPlayerStatus deriving (Show)

type LocalPlayerList = [LocalPlayer]

updateLocalPlayers :: LocalPlayerList -> [Player] -> LocalPlayerList
updateLocalPlayers localPlayers worldPlayers =
  -- Cleanup leftovers and mark Added as retained (removed in previous message)
  let cleaned = mapMaybe advanceLocalPlayer_ localPlayers
   in -- process removed for this message
      let withRemoved = map (processRemoval_ worldPlayers) cleaned
       in -- process added, up to 15 new players per message
          let withAdded = processAddition_ withRemoved worldPlayers
           in withAdded

advanceLocalPlayer_ :: LocalPlayer -> Maybe LocalPlayer
advanceLocalPlayer_ (LocalPlayer p Added) =
  Just (LocalPlayer p Retained)
advanceLocalPlayer_ (LocalPlayer _ Removed) =
  Nothing
advanceLocalPlayer_ (LocalPlayer p Retained) =
  Just (LocalPlayer p Retained)

processRemoval_ :: [Player] -> LocalPlayer -> LocalPlayer
processRemoval_ worldPlayers local =
  let (LocalPlayer p status) = local in
    case newReference_  local worldPlayers of
      Nothing -> LocalPlayer p Removed
      Just p -> LocalPlayer p status

newReference_ :: LocalPlayer -> [Player] -> Maybe Player
newReference_ (LocalPlayer p status) = find (\x -> username x == username p)

processAddition_ :: [LocalPlayer] -> [Player] -> [LocalPlayer]
processAddition_ currentLocalPlayers worldPlayers =
  let newPlayers = filter (not . isAlreadyKnown_ currentLocalPlayers) worldPlayers
   in currentLocalPlayers ++ take 15 (map (`LocalPlayer` Added) newPlayers)

isAlreadyKnown_ :: [LocalPlayer] -> Player -> Bool
isAlreadyKnown_ known other =
  case find (\(LocalPlayer p _) -> username p == username other) known of
    Nothing -> False
    Just _ -> True
