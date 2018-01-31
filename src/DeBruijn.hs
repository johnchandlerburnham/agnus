module DeBruijn where

-- Peano numbers
data Index = Z | S Index deriving (Eq, Show)

instance Ord Index where
  (<=) Z _ = True
  (<=) _ Z = False
  (<=) (S a) (S b) = (<=) a b

instance Enum Index where
  toEnum int = if int <= 0 then Z else S (toEnum (int - 1))
  fromEnum Z = 0
  fromEnum (S ind) = 1 + fromEnum ind

-- successor
suc :: Index -> Index
suc = S

-- predecessor
pre :: Index -> Index
pre Z = Z
pre (S n) = n

-- addition
add :: Index -> Index -> Index 
add a Z = a
add Z b = b
add (S a) b = S (add a b)

-- ordering

-- Lambda Expressions
-- A term is either an index or a pair of terms
data Term = I Index | P Term Term deriving Eq

instance Show Term where
  show (I n) = show $ fromEnum n
  show (P a b) = "(" ++ (show a) ++ " " ++ show b ++ ")"

-- shift all free indices with respect to depth r by depth d
-- e.g. the 3 in (0 3) is free with respect to depths 0 and 1
shift :: Index -> Index -> Term -> Term
shift r d (P (I Z) b) = P (I Z) (shift (suc r) d b)
shift r d (P a b) = P (shift r d a) (shift r d b)
shift r d (I n) = if n > r then I (add n d) else I n

-- Substitute depth d indices with term x in the body of an abstraction
subst :: Index -> Term -> Term -> Term
subst d x (I n) =
  case compare n d of
    LT -> I n
    EQ -> shift Z (pre d) x
    GT -> I (pre n)
subst d x (P (I Z) b) = P (I Z) (subst (suc d) x b)
subst d (I Z) (P (I n) b) = 
  if n == d then P (I Z) (shift Z (S Z) $ subst d (I Z) b)
  else P (shift Z (pre d) (I Z)) (subst d (I Z) b)
subst d x (P a b) = P (subst d x a) (subst d x b)

-- Normal-Order-Reduction
reduce :: Term -> Term  
reduce (P (P (I Z) f) x) = subst (S Z) x f
reduce (P l r) = P (reduce l) r
reduce term = term


