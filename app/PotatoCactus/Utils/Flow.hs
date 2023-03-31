module PotatoCactus.Utils.Flow ((|>)) where

infixl 0 |>

(|>) :: a -> (a -> b) -> b
x |> f = apply x f

apply :: a -> (a -> b) -> b
apply x f = f x

-- TODO - Simplify nested let a = (...) in ( let b = a (...))  - keotl 2023-03-31
