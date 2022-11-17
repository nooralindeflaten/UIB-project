module Week41Exercise2 where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

data Expr a
  = Var a
  | Lit Integer
  | Mul (Expr a) (Expr a)
  | Add (Expr a) (Expr a)
  deriving (Eq, Show)


eval :: (Ord variable, Num value) => Expr variable -> Map variable value -> Maybe value
eval (Var i) m = Map.lookup i m
eval (Lit i) m = Just (fromInteger i)
eval (Mul e1 e2) m = (*) <$> (eval e1 m) <*> (eval e2 m)
eval (Add e1 e2) m = (+) <$> (eval e1 m) <*> (eval e2 m)