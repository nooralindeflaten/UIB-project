module Week40Exercise0 where

data Palindrome =
    Word String --String literal
    | HalfPali Palindrome -- First half of Palindrome
    | MiddleChar Palindrome
    deriving (Eq, Read, Show)


eval :: Palindrome -> String
eval (Word s) = s
eval (HalfPali s) = take (length (eval s) `div` 2) (eval s)
eval (MiddleChar s) = if (length s' `mod` 2 /= 0)
    then show (s' !! (length s' `div` 2))
    else ""
    where s' = eval s


palindrome :: String -> Maybe Palindrome
palindrome e1 = if e1 == (reverse e1)
    then Just (Word e1)
    else Nothing
    

{-
Palindrome data 
- lagrer dataen til et palindrome
- første halvparten av ordet.
- og midtbokstaven hvis ordet har oddetallslengde. 

- check palindrome. så String literal.
- 
-}