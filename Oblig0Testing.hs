module Oblig0Testing where

average :: (Fractional a) => [a] -> a
average xs = sum xs / fromIntegral (length xs)


zeroCrossings :: (Num a, Ord a) => [a] -> Integer
zeroCrossings [] = 0
zeroCrossings [_] = 0
zeroCrossings (x:y:xs)
    | (x <= 0 && y > 0) || (x > 0 && y <= 0) = 1+ zeroCrossings (y:xs)
    | otherwise = zeroCrossings (y:xs)

extend ::  Num a => [a] -> [a]
extend [] = repeat 0
extend [xs] = repeat xs
extend (x:xs) = x : extend xs

lpf :: (Fractional a) => Integer -> [a] -> a
lpf n xs = average xs' where xs' = take (fromIntegral n) xs

hpf :: (Fractional a) => Integer -> [a] -> a
hpf n xs = (lpf n xs) - (head xs)
