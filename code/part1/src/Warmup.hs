module Warmup where

-- ----------------------------------------------------------------------------
{-# OPTIONS_GHC -W #-}  -- Just in case you forgot...

type Pos = (Int, Int)

data Direction = North | South | East | West
  deriving (Eq, Show, Read, Ord)


move :: Direction -> Pos -> Pos
move North (x,y) = (x,   y+1)
move West  (x,y) = (x-1, y  )
move South (x,y) = (x,   y-1)
move East  (x,y) = (x+1, y  )


moves :: [Direction] -> Pos -> Pos
moves dirs p0 = foldr move p0 dirs


-- ----------------------------------------------------------------------------

data Nat = Zero | Succ Nat
  deriving (Eq, Show, Read, Ord)


add :: Nat -> Nat -> Nat
add x Zero     = x
add x (Succ y) = add (Succ x) y


mult :: Nat -> Nat -> Nat
mult x y = f x y Zero where
  f a Zero     c    = c
  f a (Succ b) c    = f a b (add a c)


-- Do not use these to define add/mult!
nat2int :: Nat -> Int
nat2int Zero     = 0
nat2int (Succ a) = 1 + nat2int a

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat x = Succ $ int2nat (x-1)

-- ----------------------------------------------------------------------------

data Tree = Leaf | Node Int Tree Tree
  deriving (Eq, Show, Read, Ord)


insert :: Int -> Tree -> Tree
insert x Leaf         = Node x Leaf Leaf
insert x (Node y l r) = if x > y 
  then Node y l $ insert x r 
  else Node y (insert x l) r


-- The polymorphic variant, to avoid name clashes with the above
data PTree a = PLeaf | PNode a (PTree a) (PTree a)
  deriving (Eq, Show, Read, Ord)


--pinsert :: FIXME  -- uncomment and replace with the proper type of pinsert
pinsert x PLeaf         = PNode x PLeaf PLeaf
pinsert x (PNode y l r) = if x > y 
  then PNode y l $ pinsert x r 
  else PNode y (pinsert x l) r
