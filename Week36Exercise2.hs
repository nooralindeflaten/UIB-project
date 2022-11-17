module Week36Exercise2 where

halfPalindrome :: String -> Maybe String
halfPalindrome s = if s' == s
    then Just (take (length s `div` 2) s)
    else Nothing 
    where s' = reverse s

decomposePalindrome :: String -> Maybe (String, Maybe Char)
decomposePalindrome s = if s' == s
    then Just ((take(length s `div` 2) s) ,(
        if (length s `mod` 2 /= 0) then Just (s !! ((length s `div` 2)))
        else Nothing))
    else Nothing
    where s' = reverse s

createPalindrome :: String -> Maybe Char -> String
createPalindrome s Nothing = s ++ reverse s
createPalindrome s (Just c) = s ++ (c : reverse s)