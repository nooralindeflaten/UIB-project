module Week38Exercise1 where

mySplitAt :: Integer -> [a] -> ([a],[a])
mySplitAt _ [] = ([],[])
mySplitAt 0 xs = ([],xs)
mySplitAt n xs
    | n < 1 = ([],xs)
    | otherwise = 
        let list1 = drop (fromIntegral n) xs
            list2 = take (fromInteger n) xs
        in (list2,list1)