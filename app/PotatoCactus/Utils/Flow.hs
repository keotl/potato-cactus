module PotatoCactus.Utils.Flow ((|>)) where

infixl 0 |>

(|>) :: a -> (a -> b) -> b
x |> f = apply x f

apply :: a -> (a -> b) -> b
apply x f = f x
