-------------------------------------------------------------------------------
-- Student's name:
-- Student's group:
--
-- Data Structures. Grado en Informática. UMA.
-------------------------------------------------------------------------------

module DataStructures.Graph.EulerianCycle(isEulerian, eulerianCycle) where

import DataStructures.Graph.Graph
import Data.List

--H.1)
isEulerian :: Eq a => Graph a -> Bool
isEulerian g 
    | isEmpty g = True
    | length (vertices g) == 1 = True
    | length (vertices g) == 2 = False
    | otherwise = isEulerian' (vertices g)
        where
            isEulerian' [] = True
            isEulerian' (x:xs)
                | even (degree g x) = isEulerian' xs
                | otherwise = False
-- H.2)
{-remove :: (Eq a) => Graph a -> (a,a) -> Graph a
remove g (v,u) = remove' (vertices g') g' 
    where
        g' = deleteEdge g (v,u)
        remove' (x:xs) g'
            | (degree g' x) == 0 = remove' xs (deleteVertex g' x)
            | otherwise = remove' xs g'-}


remove :: (Eq a) => Graph a -> (a,a) -> Graph a
remove g (v,u)
    | (degree g' v) == 0 && (degree g' u) /= 0 = (deleteVertex g' v)
    | (degree g' u) == 0 && (degree g' v) /= 0 = (deleteVertex g' u)
    | (degree g' u) == 0 && (degree g' v) == 0 = deleteVertex (deleteVertex g' u) v
    | otherwise = g'
    where
        g' = deleteEdge g (v,u)

-- H.3)
extractCycle :: (Eq a) => Graph a -> a -> (Graph a, Path a)
extractCycle g v0 = aux [v0] v0 (head(successors g v0)) g
    where
        aux ciclo v u g
            | v0 == u = (g',ciclo ++ [v0])
            | otherwise = aux (ciclo ++ [u]) u (head(successors g' u)) (g')
                where
                    g' = remove g (v,u) 

-- H.4)
connectCycles :: (Eq a) => Path a -> Path a -> Path a
connectCycles [] ys = ys
connectCycles (x:xs) (y:ys)
    | x == y = [x] ++ ys ++ xs
    | otherwise = [x] ++ connectCycles xs (y:ys)

-- H.5)
vertexInCommon :: Eq a => Graph a -> Path a -> a
vertexInCommon g cycle = (head[x | x <- (vertices g), elem x cycle])

-- H.6) 
eulerianCycle :: Eq a => Graph a -> Path a
eulerianCycle g
    | not(isEulerian g) = error "el grafo no es euleriano"
    | isEmpty g = [] 
    | length (vertices g) == 1 = [head(vertices g)]
    | otherwise = aux g' p
        where
            (g',p) = extractCycle g (head(vertices g))
            aux g p
                | isEmpty g = p
                | otherwise = aux (fst grafonuevo) (connectCycles p (snd grafonuevo))
                    where
                        verticecomun = vertexInCommon g p
                        grafonuevo = (extractCycle g verticecomun)







data Vertex = A | B | C | D | E | F | G | H | I | J deriving (Show,Eq,Enum,Ord)

g0n :: Graph Vertex -- not eulerian
g0n = mkGraphEdges [] []

g1 :: Graph Vertex -- eulerian
g1 = mkGraphEdges [A] []

g2n :: Graph Vertex -- not eulerian
g2n = mkGraphSuc [A,B] suc
    where
      suc A = [B]
      suc B = [A]

g3 :: Graph Vertex -- eulerian
g3 = mkGraphEdges [A .. C]
                  [(A,B), (B,C), (C,A)]

g5 :: Graph Vertex
g5 = mkGraphEdges [A .. E]
                  [(A, C), (A, D), (B, C), (B, E), (C, D), (C, E)]

g6 :: Graph Vertex
g6 = mkGraphSuc vertices suc -- eulerian
    where
      vertices = [A .. F]
      suc A = [B,E]
      suc B = [A,C,D,E]
      suc C = [B,D,E,F]
      suc D = [B,C,E,F]
      suc E = [A,B,C,D]
      suc F = [C,D]

{-
  A--B--D--F
     |  |
     C--E
-}
g6n  = mkGraphEdges vertices edges -- not eulerian
    where
      vertices =  [A .. F]
      edges = [(A,B),(B,C),(B,D),(D,E),(D,F),(C,E)]

{-
  A--B--D--F
   \ |  |
     C--E--G
-}
g7n  = mkGraphSuc vertices suc -- not eulerian
    where
      vertices = [A .. G]
      suc A = [B,C]
      suc B = [A,C,D]
      suc C = [A,B,E]
      suc D = [B,F,E]
      suc E = [C,D,G]
      suc F = [D]
      suc G = [E]

g10 = mkGraphSuc vertices suc -- eulerian
    where
      vertices = [A .. J]
      suc A = [E,J,I,B]
      suc B = [A,I,H,C]
      suc C = [D,G,H,B]
      suc D = [E,F,G,C]
      suc E = [D,F,J,A]
      suc F = [E,D,G,J]
      suc G = [D,C,H,F]
      suc H = [G,I,C,B]
      suc I = [A,B,J,H]
      suc J = [E,F,A,I]



