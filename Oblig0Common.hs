module Oblig0Common where

-- Compute the average of a list of values
average :: (Fractional a) => [a] -> a
average xs = sum xs / fromIntegral (length xs)
-- A simple lowpass filter with adjustable cut-off
lpf :: (Fractional a) => Integer -> [a] -> a
lpf n xs = average xs' where xs' = take (fromIntegral n) xs

-- A simple high pass filter with adjustable cut-off
hpf :: (Fractional a) => Integer -> [a] -> a
hpf n xs = (lpf n xs) - (head xs)

-- Extend a finite signal with an infinite constant past
extend ::  Num a => [a] -> [a]
extend [] = repeat 0
extend [xs] = repeat xs
extend (x:xs) = x : extend xs


-- Apply a filter to a list of values
applyFilter :: (Num a, Floating a) => ([a] -> a) -> [a] -> [a]
applyFilter fil = map fil . iterate tail . extend


-- Count the number of zero-crossings in a signal represented by a list
zeroCrossings :: (Num a, Ord a) => [a] -> Integer
zeroCrossings [] = 0
zeroCrossings [_] = 0
zeroCrossings (x:y:xs)
    | (x <= 0 && y > 0) || (x > 0 && y <= 0) = 1+ zeroCrossings (y:xs)
    | otherwise = zeroCrossings (y:xs)

lowPassCutoff :: Integer
lowPassCutoff = 10
highPassCutoff :: Integer
highPassCutoff = 7
            

