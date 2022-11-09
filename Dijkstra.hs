module Dijkstra
(
    fromText,
    dijkstra,
    pathToNode,
    edgesFor,
    Edge(..),
    Node,
    Graph,
    Dnode
) where

import Data.List

{-
number of test cases or lines to read = 2N
length = first part. 
so from the input we get line 1. 2*n = number of roads
3
4
20 20 20 20

6
3 2 5 3 1 2
3+2 -5 = 0. 0+3
uuduud
7
3 4 2 1 6 4 5
-}

data Edge =
    Edge { node::Node, weight::Float } deriving (Show)
type Node = String
type Graph = [(Node, [Edge])]
type Dnode = (Node, (Float, Node))

data Climb = 
    

fromText :: String -> Bool -> Graph
fromText strLines isDigraph =
    let readData [n1, n2, w] = ((n1, n2), read w :: Float)
        es = map (readData . words) $ lines strLines
        allEs = if isDigraph then es
                else appendReversed es
    in fromList allEs

appendReversed :: [((String, String), Float)] -> [((String, String), Float)]
appendReversed es = es ++ map (\((n1,n2),w) -> ((n2,n1),w)) es

fromList :: [((String, String), Float)] -> Graph
fromList es =
    let nodes = nub . map(fst . fst) $ es
        edgesFor es node =
            let connected = filter (\((n, _), _) -> node == n) $ es
            in map (\((_,n),wt) -> Edge n wt) connected
    in map (\n -> (n, edgesFor es n)) nodes

edgesFor :: Graph -> Node -> [Edge]
edgesFor g n = snd . head . filter(\(nd, _) -> nd == n) $ g

weightFor :: Node -> [Edge] -> Float
weightFor n = weight . head . filter(\e -> n == node e)

connectedNodes :: [Edge] -> [Node]
connectedNodes = map node

dnodeForNode :: [Dnode] -> Node -> Dnode
dnodeForNode dnodes n = head . filter(\(x, _) -> x == n) $ dnodes

dijkstra :: Graph -> Node -> [Dnode]
dijkstra g start =
    let dnodes = initD g start
        unchecked = map fst dnodes
    in dijkstra' g dnodes unchecked

initD :: Graph -> Node -> [Dnode]
initD g start =
    let initDist (n, es)
          | n == start = 0
          | start `elem` connectedNodes es = weightFor start es
          | otherwise = 1.0/0.0
    in map (\pr@(n, _) -> (n, (initDist pr, start))) g

dijkstra' :: Graph -> [Dnode] -> [Node] -> [Dnode]
dijkstra' g dnodes [] = dnodes
dijkstra' g dnodes unchecked =
    let dunchecked = filter (\dn -> fst dn `elem` unchecked) dnodes
        current = minimumBy (\(_,(d1,_)) (_,(d2,_)) -> compare d1 d2) dunchecked
        c = fst current
        unchecked' = delete c unchecked
        edges = edgesFor g c
        cnodes = intersect (connectedNodes edges) unchecked'
        dnodes' = map(\dn -> update dn current cnodes edges) dnodes
    in dijkstra' g dnodes' unchecked'

update :: Dnode -> Dnode -> [Node] -> [Edge] -> Dnode
update dn@(n,(nd, p)) (c, (cd, _)) cnodes edges =
    let wt = weightFor n edges
    in if n `notElem` cnodes then dn
        else if cd+wt < nd then (n, (cd+wt, c)) else dn

pathToNode :: [Dnode] -> Node -> [Node]
pathToNode dnodes dest =
    let dn@(n, (d,p)) = dnodeForNode dnodes dest
    in if n == p then [n] else pathToNode dnodes p++ [n]






