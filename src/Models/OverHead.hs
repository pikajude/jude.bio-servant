module Models.OverHead where

overHead :: (a -> a) -> [a] -> [a]
overHead f (x:xs) = f x : xs
overHead _ [] = []
