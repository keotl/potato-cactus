module PotatoCactus.Utils.Iterable where

replace :: (t -> Bool) -> t -> [t] -> [t]
replace _ _ [] = []
replace predicate replacement (x : xs) =
  if predicate x
    then replacement : replace predicate replacement xs
    else x : xs

replaceAtIndex :: Int -> t -> [t] -> [t]
replaceAtIndex _ _ [] = []
replaceAtIndex index replacement old =
  let (x, _ : ys) = splitAt index old
   in x ++ replacement : ys

alterAtIndex :: Int -> (t -> t) -> [t] -> [t]
alterAtIndex index transform old =
  let (x, oldValue : ys) = splitAt index old
   in x ++ transform oldValue : ys
