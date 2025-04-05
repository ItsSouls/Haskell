-------------------------------------------------------------------------------
-- Student's name:
-- Student's group:
--
-- Data Structures. Grado en Informática. UMA.
-------------------------------------------------------------------------------

{-module DataStructures.Graph.EulerianCycle where

import DataStructures.Graph.Graph
import Data.List

--H.1)
isEulerian :: Eq a => Graph a -> Bool
isEulerian g = all even [degree g v | v <- (vertices g)] == True 

-- H.2)

    
-- H.3)
extractCycle :: (Eq a) => Graph a -> a -> (Graph a, Path a)
extractCycle g v0
    | null (successors g) = error "no successors found"
    | otherwise = extractCycle' g [v0] v0 u
    where
        u = head (successors g v0)
        extractCycle' g cycle v suc
            | null (successors g) = error "no successors found"
            | suc == v0 = (newGraph, cycle ++ [v0])
            | otherwise = extractCycle' newGraph (cycle ++ [suc]) suc newSuc
                where
                    newGraph = remove g (v,suc)
                    newSuc = if nullhead (successors newGraph suc)
-}
module DataStructures.Graph.EulerianCycle where

import DataStructures.Graph.Graph
import Data.List
-- H-1)
isEulerian :: Eq a => Graph a -> Bool
isEulerian g
    |isEmpty g || length (vertices g) == 1 = True
    |all even (map (degree g) (vertices g)) = True
    |(length (vertices g) == 2) && all (>2) (map (degree g) (vertices g)) = True
    | otherwise = False

-- H-2)
remove :: (Eq a) => Graph a -> (a,a) -> Graph a
remove g (v,u) = remove' (vertices g') g'
    where
        g' = deleteEdge g (v, u)
        remove' [] g = g
        remove' (x:xs) g
            | (degree g x) == 0 = remove' xs (deleteVertex g x)
            | otherwise = remove' xs g


-- H.3)
extractCycle :: (Eq a) => Graph a -> a -> (Graph a, Path a)
extractCycle g v0 = extractCycle' g [v0] v0 (head (successors g v0))
    where
        extractCycle' g cycle v suc
            | suc == v0 = (newGraph, cycle ++ [v0])
            | otherwise = extractCycle' newGraph (cycle ++ [suc]) suc newSuc
                where
                    newGraph = (remove g (v,suc))
                    newSuc = head (successors newGraph suc)

-- H-4)
connectCycles :: (Eq a) => Path a -> Path a -> Path a
connectCycles [] ys = ys
connectCycles xs [] = xs
connectCycles xs (y:ys)
    | (head xs) == y = [head xs] ++ ys ++ (tail xs)
    | otherwise = [head xs] ++ connectCycles (tail xs) (y:ys)


-- H-5)
vertexInCommon :: Eq a => Graph a -> Path a -> a
vertexInCommon g cycle = head [v | v <- cycle, v `elem` vertices g]


-- H.6) 
eulerianCycle :: Eq a => Graph a -> Path a
eulerianCycle g
    | not (isEulerian g) = error "el grafo no es euleriano"
    | isEmpty g = []
    | length (vertices g) == 1 = [head (vertices g)]
    | otherwise = eulerianCycle' g (head(vertices g))
        where
            eulerianCycle' g v
                | isEmpty g = []
                | otherwise = connectCycles cycle (eulerianCycle' g' newVertex)
                    where
                        g' = fst(extractCycle g v)
                        cycle = snd(extractCycle g v) 
                        newVertex = vertexInCommon g' cycle