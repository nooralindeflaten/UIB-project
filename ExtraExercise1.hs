module ExtraExercise1 where

takeMaybe :: Integer -> [a] -> Maybe [a]
takeMaybe n  xs = if n < 0 || n > fromIntegral (length xs) then Nothing
    else Just (take (fromIntegral n) xs)