﻿----------------------------------------------
-- Estructuras de Datos.  2011/12.
-- 2º Curso del Grado en Ingeniería [Informática | del Software | de Computadores].
-- Escuela Técnica Superior de Ingeniería en Informática. UMA
--
-- Examen 13-Febrero-2012
-- Estudio de grafos bipartitos por coloreado con búsqueda en profundidad
-- 
-- ALUMNO :  
-- GRADO:  
--  
----------------------------------------------

module BiPartite(
    biColored    -- :: Ord v  => Graph v -> Maybe (D.Dictionary v Color)
  , Color(..)
  )  where

import Graph 
import Data.Maybe(isJust)
import qualified Dictionary as D
import qualified Stack as S


data Color = Red |  Blue deriving (Eq,Show,Ord)
nextColor Red  = Blue
nextColor Blue = Red

pushAll :: S.Stack a -> [a] -> S.Stack a
pushAll = foldr S.push   

biColored :: Ord v => Graph v -> Maybe (D.Dictionary v Color)
biColored g 
 | null vs   = Just D.empty -- empty graph is bipartite
 | otherwise = aux g D.empty (S.push (src ,Red) S.empty)
 where 
  vs  = vertices g
  src = head vs -- initial vertex
 
aux :: Ord v => Graph v -> D.Dictionary v Color -> S.Stack (v, Color) -> 
                Maybe (D.Dictionary v Color)
aux g dict stack
  | S.isEmpty stack       = Just dict
  | not (D.isDefinedAt k dict) = aux g (D.insert k v dict) (pushAll (S.pop stack) lista)
  | D.valueOf k dict /= Just v = Nothing  
  | D.valueOf k dict == Just v = aux g dict (S.pop stack)
  where
    (k,v) = S.top stack
    lista = [(x, nextColor v) | x <- Graph.successors g k]
    colored  v         = D.isDefinedAt v dict
-- ¡¡¡ completad el resto de guardas !! 

     
--
-- ¡¡¡ completad las variables locales necesarias !!!
--
 
---------------------
--- EXAMPLES --------
---------------------
data MiVertice = A|B|C|D|E|F|G deriving (Show,Eq,Enum,Ord)

{- 
  A--B--D--F    
   \ |  |
     C--E--G
-}
g1  = mkGraphSuc vertices suc 
  where 
        vertices = [A .. G]
        suc A = [C,B]
        suc B = [A,C,D]
        suc C = [B,E]
	suc D = [B,F,E]
        suc E = [C,D,G]
        suc F = [D,D]
	suc G = [E]
-- *BiPartite> biColored g1
-- Nothing

{- 
  A--B--D--F    
     |  |
     C--E
-}
g2  = mkGraphEdges vertices edges 
  where 
        vertices =  [A .. F]
        edges = [(A,B),(B,C),(B,D),(D,E),(D,F),(C,E)]
-- *BiPartite> biColored g2
-- Just Dictionary(A->Red,B->Blue,C->Red,D->Red,E->Blue,F->Blue)

-- construcción de los bipartitos K n m
k n m = mkGraphEdges vertices edges
  where 
            vertices = [1 .. n + m]
            edges = [ (r,a) | r<-[1..n], a<-[n+1..n+m] ] 

-- *BiPartite> biColored (k 2 3)
-- Just Dictionary(1->Red,2->Red,3->Blue,4->Blue,5->Blue)
