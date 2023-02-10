module PotatoCactus.Utils.Iterable where

replace :: (t -> Bool) -> t -> [t] -> [t]
replace _ _ [] = []
replace predicate replacement (x : xs) =
  if predicate x
    then replacement : replace predicate replacement xs
    else x : xs
