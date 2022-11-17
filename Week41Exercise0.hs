module Week41Exercise0 where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe

type Graph n = Map n (Set n)

{-
sette to noder i en graf
skal settes edge fra node n til node 2
hvis noden ikke finnes så skal det lages en ny
og dersom den du får først ikke er der må vi lage en ny som går til den andre
og motsatt hvis ikke. 


check if node 1 is in graph. 
-}


insertEdge :: (Ord n) => n -> n -> Graph n -> Graph n
insertEdge n1 n2 g1 =
    let k = fromMaybe Set.empty $ Map.lookup n1 g1
    -- node1 set first as empty. then look if node1 is in g. if it is. 
        -- let k = the empty set i made + maybe the child nodes in node 1. 
        k' = Map.fromList [(n1, Set.insert n2 k)]
        l = Map.fromList [(n2,fromMaybe Set.empty $ Map.lookup n2 g1)]
    in Map.union (Map.union k' l) g1

    




