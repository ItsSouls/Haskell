----------------------------------------------
-- Estructuras de Datos.  2018/19
-- 2º Curso del Grado en Ingeniería [Informática | del Software | de Computadores].
-- Escuela Técnica Superior de Ingeniería en Informática. UMA
--
-- Examen 4 de febrero de 2019
--
-- ALUMNO/NAME:
-- GRADO/STUDIES:
-- NÚM. MÁQUINA/MACHINE NUMBER:
--
----------------------------------------------

module Kruskal(kruskal, kruskals) where

import qualified DataStructures.Dictionary.AVLDictionary as D
import qualified DataStructures.PriorityQueue.LinearPriorityQueue as Q
import DataStructures.Graph.DictionaryWeightedGraph

kruskal :: (Ord a, Ord w) => WeightedGraph a w -> [WeightedEdge a w]
kruskal dict = aux (dictrep(vertices dict)) (pqaux(edges dict)) []
    where 
        aux dictrep pq t
            | Q.isEmpty (pq) = t
            | (representante v1 dictrep) /= (representante v2 dictrep) = aux (D.insert (representante v2 dictrep) v1 dictrep) (Q.dequeue pq) (e:t)
            | otherwise = aux dictrep (Q.dequeue pq) t
                where
                    e@(WE v1 w v2) = Q.first (pq)

dictrep :: (Ord a) => [a] -> D.Dictionary a a
dictrep l = aux l D.empty
    where
        aux [] dict = dict
        aux (x:xs) dict = aux xs (D.insert x x dict)

pqaux :: (Ord w, Eq a) => [WeightedEdge a w] -> Q.PQueue (WeightedEdge a w)
pqaux l = Q.mkPQueue l

representante :: (Ord a) => a -> D.Dictionary a a -> a
representante v dict
    | v == valor = v
    | otherwise = representante valor dict
        where Just valor = D.valueOf v dict

-- Solo para evaluación continua / only for part time students
kruskals :: (Ord a, Ord w) => WeightedGraph a w -> [[WeightedEdge a w]]
kruskals = undefined

g1 :: WeightedGraph Char Int
g1 = mkWeightedGraphEdges ['a','b','c','d','e']
                            [ WE 'a' 3 'b', WE 'a' 7 'd'
                            , WE 'b' 4 'c', WE 'b' 2 'd'
                            , WE 'c' 5 'd', WE 'c' 6 'e'
                            , WE 'd' 5 'e'
                            ]
-- kruskal g1
