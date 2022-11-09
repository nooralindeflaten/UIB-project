module ExtraExercise0 where

doubleNames :: [String] -> [String]
doubleNames names = [x ++ "-" ++ y | x <- names, y <- names, x /= y, head x /= head y]