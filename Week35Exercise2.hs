module Week35Exercise2 where

repeat2 :: [a] -> [a]
repeat2 xs = xs' where xs' = xs ++ xs
