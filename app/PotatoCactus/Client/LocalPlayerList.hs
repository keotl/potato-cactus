module PotatoCactus.Client.LocalPlayerList (LocalPlayerList, LocalPlayerStatus (Added, Removed, Retained), LocalPlayer, updateLocalPlayers) where

import Data.Maybe (catMaybes, isNothing)
import PotatoCactus.Config.Constants (entityViewingDistance)
import PotatoCactus.Game.Player (Player)
import PotatoCactus.Game.Position (GetPosition (getPosition), Position, isWithin)
import PotatoCactus.Game.Typing (Advance (advance))

data LocalPlayerStatus = Added | Removed | Retained

data LocalPlayer = LocalPlayer Player LocalPlayerStatus

data LocalPlayerList = LocalPlayerList [LocalPlayer]

-- Process state transitions between ticks. Does not check current local players
instance Advance LocalPlayerList where
  advance (LocalPlayerList list) =
    let advanced = map advanceLocalPlayer_ list
     in LocalPlayerList $ catMaybes advanced

updateLocalPlayers :: LocalPlayerList -> [Player] -> LocalPlayerList
updateLocalPlayers (LocalPlayerList current) players =
  let updated = map markRemoved_ current in
    
    LocalPlayerList ([])

markRemoved_ :: LocalPlayer -> LocalPlayer
markRemoved_ (LocalPlayer p _) = LocalPlayer p Removed

advanceLocalPlayer_ :: LocalPlayer -> Maybe LocalPlayer
advanceLocalPlayer_ (LocalPlayer p Added) =
  Just (LocalPlayer p Retained)
advanceLocalPlayer_ (LocalPlayer _ Removed) =
  Nothing
advanceLocalPlayer_ (LocalPlayer p Retained) =
  Just (LocalPlayer p Retained)

-- updatePlayer_ :: Position -> LocalPlayer -> LocalPlayer
-- updatePlayer_ refPos (LocalPlayer p status) =
--   if isWithin entityViewingDistance refPos (getPosition  p) then
--     Lo
