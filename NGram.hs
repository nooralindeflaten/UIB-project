module NGram (NGram,Weight
             ,grams
             ,gramsWithNext
             ,combineGrams
             ,updateGram) where
import Control.Monad
import Data.List
import Control.Arrow

import qualified Data.Map as Map
import Data.Map (Map)

-- Rename types to clarify some type signatures later
type NGram = String
type Weight = Integer

-- Produce all n-grams contained in a given string
grams :: Integer -> String -> [NGram]
grams n s
    | length s >= (fromIntegral n) = take (fromIntegral n) s : grams n (tail s)
    | otherwise = []

-- Produce all n-grams contained in a given string, paired
-- with the subsequent character
gramsWithNext :: Integer -> String -> [(NGram,Char)]
gramsWithNext n s
    | length s < (fromIntegral n) = []
    | otherwise = 
        let xs = grams n s
            ys = map last (tail xs)
        in
            zip (init xs) ys
-- Recombine a list of n-grams to a string
combineGrams :: [NGram] -> String
combineGrams xs
    | length xs >= 1 = map head (init xs) ++ last xs
    | otherwise = []
        

-- Update an n-gram by adding a character to the end
-- and removing the first character.
updateGram :: NGram -> Char -> NGram
updateGram g c = tail g ++ [c]