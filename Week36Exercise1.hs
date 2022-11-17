module Week36Exercise1 where

f :: [Integer] -> Integer -> Bool
f xs y = if (g > y) then True else False where
    g = sum(xs)