{------------------------------------------------------------------------------
 - Student's name:
 -
 - Student's group:
 -----------------------------------------------------------------------------}

module AVL 
  ( 
    Weight
  , Capacity
  , AVL (..)
  , Bin
  , emptyBin
  , remainingCapacity
  , addObject
  , maxRemainingCapacity
  , height
  , nodeWithHeight
  , node
  , rotateLeft
  , addNewBin
  , addFirst
  , addAll
  , toList
  , linearBinPacking
  , seqToList
  , addAllFold
  ) where

type Capacity = Int
type Weight = Int

data Bin = B Capacity [Weight] 

data AVL = Empty | Node Bin Int Capacity AVL AVL deriving Show


emptyBin :: Capacity -> Bin
emptyBin x = B x []

remainingCapacity :: Bin -> Capacity
remainingCapacity (B c list) = c

addObject :: Weight -> Bin -> Bin
addObject x (B c list)  
  | x <= c = B (c-x) ([c] ++ list)
  | otherwise = error "el objeto no cabe en el cubo"

maxRemainingCapacity :: AVL -> Capacity
maxRemainingCapacity Empty = 0
maxRemainingCapacity (Node _ _ c _ _)  = c

height :: AVL -> Int
height Empty = 0
height (Node _ h _ _ _) = h

 
nodeWithHeight :: Bin -> Int -> AVL -> AVL -> AVL
nodeWithHeight bin@(B c list) h ln rn 
  | maxrmc > c = Node bin h maxrmc ln rn
  | otherwise = Node bin h c ln rn
    where maxrmc = max (maxRemainingCapacity ln) (maxRemainingCapacity rn)

node :: Bin -> AVL -> AVL -> AVL
node bin ln rn = nodeWithHeight bin h ln rn
    where h = (max (height ln) (height rn)) + 1

rotateLeft :: Bin -> AVL -> AVL -> AVL
rotateLeft c l r@(Node x h cap r1 r2) = nodeWithHeight x (h+1) (nodeWithHeight c h l r1) r2

addNewBin :: Bin -> AVL -> AVL
addNewBin bin@(B c list) Empty = Node bin 1 c Empty Empty
addNewBin bin@(B c list) node@(Node x h cap r1 r2)
  | height r2 - height r1 > 1 = rotateLeft x r1 (addNewBin bin r2)
  | otherwise = Node x h cap r1 (addNewBin bin r2)

addFirst :: Capacity -> Weight -> AVL -> AVL
addFirst w objw Empty = addNewBin (B (w-objw) [objw]) Empty
addFirst w objw node@(Node bin h c r1 r2)
  | (maxRemainingCapacity node) < objw = addNewBin (B (w-objw) [objw]) node
  | (maxRemainingCapacity r1) >= objw = Node bin h c (addFirst w objw r1) r2
  | (maxRemainingCapacity node) >= objw = Node (addObject objw bin) h (c-objw) r1 r2
  | otherwise = Node bin h c r1 (addFirst w objw r2)

addAll:: Capacity -> [Weight] -> AVL
addAll w [] = Empty
addAll w (x:xs) = addFirst w x (addAll w xs)

toList :: AVL -> [Bin]
toList Empty = []
toList (Node bin h c r1 r2) = toList r1 ++ [bin] ++ toList r2

{-
	SOLO PARA ALUMNOS SIN EVALUACION CONTINUA
  ONLY FOR STUDENTS WITHOUT CONTINUOUS ASSESSMENT
 -}

data Sequence = SEmpty | SNode Bin Sequence deriving Show  

linearBinPacking:: Capacity -> [Weight] -> Sequence
linearBinPacking _ _ = undefined

seqToList:: Sequence -> [Bin]
seqToList _ = undefined

addAllFold:: [Weight] -> Capacity -> AVL 
addAllFold _ _ = undefined



{- No modificar. Do not edit -}

objects :: Bin -> [Weight]
objects (B _ os) = reverse os

  
instance Show Bin where
  show b@(B c os) = "Bin("++show c++","++show (objects b)++")"