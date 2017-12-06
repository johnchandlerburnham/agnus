module Machina where

-- Lambda Expression with De Bruijn Indices and disembodied lambdas at (Index 0)
data Term = Index Integer | Pair Term Term deriving (Eq, Show)

-- Normal-Order Reduction
reduce :: Term -> Term  
reduce (Pair (Pair (Index 0) f) x) = subTerm 1 x f
reduce (Pair l r) = Pair (reduce l) r
reduce term = term

-- Substitute all indices that point to depth d with term x
subTerm :: Integer -> Term -> Term -> Term
subTerm d x (Index n) = 
  case (compare n d) of 
    LT -> Index n
    EQ -> shift (d - 1) x
    GT -> Index (n - 1)
subTerm d x (Pair (Index 0) r) = Pair (Index 0) (subTerm (d + 1) x r)
subTerm d x@(Index 0) (Pair l@(Index n) r) =  
  case (n == d) of
    True  -> Pair (Index 0) (shift 1 $ subTerm d x r)
    False -> Pair (subTerm d x l) (subTerm d x r)
subTerm d x (Pair l r) = Pair (subTerm d x l) (subTerm d x r) 

-- Shift all free indices in a term by a depth
shift :: Integer -> Term -> Term
shift depth term = go 0 term where
  go d (Index n) = if n > d then Index (n + depth) else Index n
  go d (Pair (Index 0) a) = Pair (Index 0) (go (d + 1) a)
  go d (Pair a b) = Pair (go d a) (go d b)
