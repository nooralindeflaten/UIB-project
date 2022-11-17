module Week37Exercise0 where

namesAndAges :: [String] -> [Integer] -> [String]
namesAndAges xs ys = [x' ++ " is " ++ (show y') ++ " years old" | (x',y') <- zs, y' <= 50] where
    zs = zip xs ys