module PotatoCactus.Game.PlayerUpdate.VarpSet (VarpSet, VarpId, Varp, updated, update, allValues, create) where

import Data.Binary (Word32)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Maybe (mapMaybe)
import PotatoCactus.Game.Typing (Advance (advance))

type VarpId = Int

-- 32-bit value containing flags for the client
-- The client might use these flags to show hide objects/entities, etc.
data Varp = Varp
  { varpId :: VarpId,
    value :: Word32
  }
  deriving (Show)

data VarpSet = VarpSet
  { content_ :: IntMap Varp,
    updated_ :: [Int]
  }
  deriving (Show)

create :: VarpSet
create =
  VarpSet
    { content_ = IntMap.empty,
      updated_ = []
    }

instance Advance VarpSet where
  advance set = set {updated_ = []}

updated :: VarpSet -> [Varp]
updated set =
  mapMaybe (`IntMap.lookup` content_ set) (updated_ set)

allValues :: VarpSet -> [Varp]
allValues = map snd . IntMap.toList . content_

update :: (VarpId, Word32) -> VarpSet -> VarpSet
update (varpId, value) set =
  if isIdentical_ (varpId, value) set
    then set
    else
      VarpSet
        { content_ = IntMap.insert varpId (Varp varpId value) (content_ set),
          updated_ = varpId : updated_ set
        }

isIdentical_ :: (VarpId, Word32) -> VarpSet -> Bool
isIdentical_ (varpId, newValue) set =
  case IntMap.lookup varpId (content_ set) of
    Nothing -> False
    Just stored -> newValue == value stored
