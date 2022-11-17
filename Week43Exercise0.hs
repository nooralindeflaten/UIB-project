module Week43Exercise0 where
import Data.Foldable
import Prelude

data BinSearchTree a
  = Empty
  | Branch (BinSearchTree a) a (BinSearchTree a)
  deriving (Eq, Show)



instance Foldable BinSearchTree where
foldr :: (a -> b -> b) -> b -> BinSearchTree a -> b
foldr f z (Branch left node right) = foldr f (node `f` foldr f z right) left
foldr _ z Empty = z
  
  

toList :: BinSearchTree a -> [a]
toList t =  foldr 