module Week41Exercise1 where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe

type Graph n = Map n (Set n)

disjoint :: (Ord a) => Set a -> Set a -> Bool
disjoint a b = Set.null (Set.intersection a b)

data MyNode = A | B | C
  deriving (Eq,Ord,Show)

graph0 :: Graph MyNode
graph0 = Map.fromList [(A,Set.fromList [A,B])
                      ,(B,Set.fromList [C])
                      ,(C,Set.fromList [A])]


 
-- if there are no more path's to check. all children are inn the visited.
-- if the node is not in the graph. or this means the node is empty then false.
paths :: (Ord n) => n -> n -> Graph n -> Set n -> Maybe[n]
paths startNode endNode g visited
-- the path from c to a, and then a to next
    | Set.member startNode visited = Nothing
    | startNode == endNode = Just []
    | otherwise = do
            let visited' = Set.insert startNode visited
            nexts <- Map.lookup startNode g
            listToMaybe
                $ mapMaybe (\next -> do
                            pathCont <- paths next endNode g visited'
                            Just (next:pathCont))
                    (Set.toList nexts)

path :: (Ord node) => Graph node -> node -> node -> Maybe [node]
path g start end = paths start end g Set.empty        

allPaths :: (Ord n) => n -> Graph n -> [Maybe[n]]
allPaths start g = 
    let kids = fromMaybe Set.empty $ Map.lookup start g
        visited = [(fromMaybe Set.empty $ Map.lookup h g) | h <- (Set.toList kids)]
        vis = map (\next -> (Set.toList next)) visited
        v = (\z l -> foldr (\x y -> foldr z y x) l vis) (:) []
    in [path g s e | s <- (Set.toList kids), e <- v]


path' :: (Ord n) => n -> Graph n -> Set n -> Set n -> Maybe[n]
path' n g visited kids
    | disjoint visited kids == False  || length (Set.toList visited) == length g = Just[]
    | otherwise 
        -- kids are now the children of the node. then check if visited is disjoint
        = do
            let visited' = Set.insert n visited
            -- now for each child we need to do this.
            kids' <- Map.lookup n g
            listToMaybe $
                mapMaybe
                (\next -> do
                    sh <- path' next g visited' kids'
                    Just(next:sh))
                (Set.toList kids')



repeated :: (Ord n) => [n] -> Bool
repeated [] = False
repeated [_] = False
repeated (h:t) = if elem h t then True
                             else repeated t
hasCycle :: (Ord n) => Graph n -> n -> Bool
hasCycle g n =
        if (repeated xs == True) 
            then True
        else False
        where xs = fromMaybe [] $ (path' n g (Set.empty) (Set.empty))