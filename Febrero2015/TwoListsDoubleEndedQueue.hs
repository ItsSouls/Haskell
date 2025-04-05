-------------------------------------------------------------------------------
-- Estructuras de Datos. Grado en Informática, IS e IC. UMA.
-- Examen de Febrero 2015.
--
-- Implementación del TAD Deque
--
-- Apellidos:
-- Nombre:
-- Grado en Ingeniería ...
-- Grupo:
-- Número de PC:
-------------------------------------------------------------------------------

module TwoListsDoubleEndedQueue
   ( DEQue
   , empty
   , isEmpty
   , first
   , last
   , addFirst
   , addLast
   , deleteFirst
   , deleteLast
   ) where

import Prelude hiding (last)
import Data.List(intercalate)
import Test.QuickCheck

data DEQue a = DEQ [a] [a]

-- Complexity:
empty :: DEQue a
empty = DEQ [] []

-- Complexity:
isEmpty :: DEQue a -> Bool
isEmpty empty = True
isEmpty _ = False

-- Complexity:
addFirst :: a -> DEQue a -> DEQue a
addFirst v empty = DEQ [v] [] 
addFirst v  (DEQ (x:xs) (y:ys))= (DEQ (v:x:xs) (y:ys))

    
-- Complexity:
addLast :: a -> DEQue a -> DEQue a
addLast v empty = DEQ [] [v] 
addLast v  (DEQ (x:xs) (y:ys))= (DEQ (x:xs) (v:y:ys))

-- Complexity:
first :: DEQue a -> a
first (DEQ [] []) = error "la cola está vacía"
first (DEQ [] ys) = head(reverse ys)
first (DEQ (x:xs) (y:ys)) = x

-- Complexity:
last :: DEQue a -> a
last (DEQ [] []) = error "la cola está vacía"
last (DEQ (x:xs) (y:ys)) = y
last (DEQ first []) = head (reverse first)

-- Complexity:
deleteFirst :: DEQue a -> DEQue a
deleteFirst (DEQ [] []) = error "la cola está vacía"
deleteFirst (DEQ (x:xs) ys) = DEQ (xs) ys
deleteFirst (DEQ [] ys) = deleteFirst (DEQ (reverse(snd (splitAt (div (length ys) 2) ys))) (fst (splitAt (div (length ys) 2) ys)))

   

-- Complexity:
deleteLast :: DEQue a -> DEQue a
deleteLast (DEQ [] []) = error "la cola está vacía"
deleteLast (DEQ xs (y:ys)) = DEQ xs ys
deleteLast (DEQ xs []) = deleteLast (DEQ (fst (splitAt (div (length xs) 2) xs)) (reverse(snd (splitAt (div (length xs) 2) xs))))




instance (Show a) => Show (DEQue a) where
   show q = "TwoListsDoubleEndedQueue(" ++ intercalate "," [show x | x <- toList q] ++ ")"

toList :: DEQue a -> [a]
toList (DEQ xs ys) =  xs ++ reverse ys

instance (Eq a) => Eq (DEQue a) where
   q == q' =  toList q == toList q'

instance (Arbitrary a) => Arbitrary (DEQue a) where
   arbitrary =  do
      xs <- listOf arbitrary
      ops <- listOf (oneof [return addFirst, return addLast])
      return (foldr id empty (zipWith ($) ops xs))


q1 :: Num a => DEQue a
q1 = DEQ [1,2,3,4,5] []

q2 :: Num a => DEQue a
q2 = addFirst 1 q1