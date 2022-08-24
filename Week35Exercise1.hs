module Week35Exercise1 where

triangleNumber :: Integer -> Integer
triangleNumber 2 = 3
triangleNumber n = sum [1..n]