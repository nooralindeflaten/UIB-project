module Week37Exercise1 where
import Data.List
{-
so 12^3 = 1728
1729 - 12^3 = 1
1^3


# find all cube numbers that are not 
12^3 = 1728
(12)
12 + 1^3 =
12 + 2^3 =
12 + 3^3 = 
-
-}
equalCubeSum :: Integer -> [(Integer,Integer,Integer,Integer)]
equalCubeSum n =  [(a,b,c,d) | (a,b):ys <- tails xs, (c,d) <- ys, (a^3 + b^3) == (c^3 + d^3), a /= c, a /= d,
                    b /= c, b /= d] where
                        xs = pairs n

pairs :: Integer -> [(Integer,Integer)]
pairs n = [(x,y) | (x:ys) <- tails xs, y <- ys] where xs = [1..n]





 