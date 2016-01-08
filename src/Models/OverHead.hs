module Models.OverHead where

overHead f (x:xs) = f x : xs
overHead f [] = []
