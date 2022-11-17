module Week38Exercise2 where
import Data.Maybe
import Text.Read
removeNothing :: [Maybe a] -> [a]
removeNothing [Nothing] = []
removeNothing [] = []
removeNothing xs =
    let ys = map(\x -> if isJust x == True then maybeToList x else []) xs
    in foldr (++) [] ys