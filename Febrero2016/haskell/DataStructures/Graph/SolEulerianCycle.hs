-------------------------------------------------------------------------------
-- Student's name:
-- Student's group:
--
-- Data Structures. Grado en Informática. UMA.
-------------------------------------------------------------------------------

module DataStructures.Graph.EulerianCycle where

import DataStructures.Graph.Graph
import Data.List

--H.1)
isEulerian :: (Eq a) => Graph a -> Bool
isEulerian g 
    |isEmpty g || length (vertices g) == 1 = True
    |length[even x | x <- grados] == length(vertices g) = True
    |otherwise = False 
        where 
            grados = [(degree g y) | y <- (vertices g)]

-- H.2)
{-remove :: (Eq a) => Graph a -> (a,a) -> Graph a
remove g (v,u) = remove' [x | x <- vertices(g'), degree g' x == 0] g'
    where 
        g' = deleteEdge g (v,u)
        remove' [] gaux = gaux
        remove' (x:xs) gaux = remove' xs (deleteVertex g' x) --probar arista una a una en g3
-}
remove :: (Eq a) => Graph a -> (a,a) -> Graph a
remove g (v,u) = remove' (vertices g') g'
    where
        g' = deleteEdge g (v, u)
        remove' [] gaux = gaux
        remove' (x:xs) gaux
            | (degree gaux x) == 0 = remove' xs (deleteVertex gaux x)
            | otherwise = remove' xs gaux

-- H.3)
extractCycle :: (Eq a) => Graph a -> a -> (Graph a, Path a)
extractCycle g v0 = extractCycle' g [v0] v0 (head(successors g v0)) 
        where 
            extractCycle' g ciclo v u
                | v0 == u = (g', ciclo ++ [v0]) 
                | otherwise = extractCycle' g' (ciclo ++ [u]) u (head(successors g' u))
                    where
                        g' = remove g (v,u)


-- H.4)
connectCycles :: (Eq a) => Path a -> Path a -> Path a
connectCycles [] ys = ys
connectCycles xs [] = xs
connectCycles (x:xs) (y:ys)
    | x == y = [x] ++ ys ++ xs  
    | otherwise = [x] ++ connectCycles xs ys

-- H.5)
vertexInCommon :: Eq a => Graph a -> Path a -> a
vertexInCommon g cycle = (head[v | v <- vertices g, elem v cycle])


-- H.6) 
eulerianCycle :: Eq a => Graph a -> Path a
eulerianCycle g
    | not (isEulerian g) = error "el grafo no es euleriano"
    | isEmpty g = []
    | length (vertices g) == 1 = [head(vertices g)]
    | otherwise = eulerianCycle' g (head(vertices g))
        where 
            eulerianCycle' g v 
                | isEmpty g = []
                | otherwise = connectCycles cicloparcial (eulerianCycle' gresultante vparcia)
                    where 
                        gresultante = fst(extractCycle g v)
                        cicloparcial = snd(extractCycle g v)
                        vparcia = (vertexInCommon gresultante cicloparcial)
