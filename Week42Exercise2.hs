module Week42Exercise2 where

reverseWords :: String -> String
reverseWords = unwords . map reverse . words

f :: Integer -> Integer -> Integer -> Maybe Bool
f a b c = if even g then Just True else Nothing
    where g = (+) b c

sumIsEven :: Integer -> Integer -> Bool
sumIsEven = (.) (maybe False id) . f 1