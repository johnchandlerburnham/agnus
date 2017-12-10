module DeBruijnDisembodied where

import Control.Applicative
import Text.ParserCombinators.ReadP
              
-- Lambda Expression with De Bruijn Indices and disembodied lambdas, (Index 0)
data Term = Index Integer | Pair Term Term deriving Eq

instance Show Term where
  show (Index n) = show n
  show (Pair a b) = "(" ++ show a ++ " " ++ show b ++ ")"

instance Read Term where
  readsPrec _ = readP_to_S parseTerm

-- Normal-Order-Reduction
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

-- shift all free indices in a term by a depth
shift :: Integer -> Term -> Term
shift depth term = go 0 term where
  go d (Index n) = if n > d then Index (n + depth) else Index n
  go d (Pair (Index 0) a) = Pair (Index 0) (go (d + 1) a)
  go d (Pair a b) = Pair (go d a) (go d b)

integer :: ReadP Integer
integer = read <$> munch1 (\c -> c >= '0' && c <= '9')

parseTerm :: ReadP Term
parseTerm = Index <$> integer <|> lambda <|> pair where
  lambda = char '\\' >> return (Index 0)
  pair = do 
    a <- char '(' >> parseTerm
    b <- char ' ' >> parseTerm
    char ')' >> return (Pair a b)

apply :: Term -> Term -> Term
apply a b = reduce (Pair a b)

apply' :: Term -> [Term] -> Term
apply' t ts = foldl apply t ts

applyC :: String -> [String] -> String
applyC c cs = show $ apply' (read c) $ read <$> cs

fullReduce :: Term -> Term
fullReduce term = go (Index 0) term where
  go a b = if a == b then a else go b (reduce b)

fullReduceList :: Term -> [Term]
fullReduceList term = go [(Index 0)] term where
  go l@(a:as) b = if a == b then l else go (b:l) (reduce b)

s :: Term
s = read "(\\ (\\ (\\ ((3 1) (2 1)))))"
-- \ \ \ 3 1 (2 1)

k :: Term
k = read "(\\ (\\ 2))"

i :: Term
i = read "(\\ 1)"

skk :: Term 
skk = apply (apply s k) k


