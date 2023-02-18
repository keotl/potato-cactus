module PotatoCactus.Utils.Iterable where

replace :: (t -> Bool) -> t -> [t] -> [t]
replace _ _ [] = []
replace predicate replacement (x : xs) =
  if predicate x
    then replacement : replace predicate replacement xs
    else x : xs

replaceAtIndex :: Int -> t -> [t] -> [t]
replaceAtIndex _ _ [] = []
replaceAtIndex 0 a (x : xs) = a : xs
replaceAtIndex i a (x : xs) =
  x : replaceAtIndex (i - 1) a xs
