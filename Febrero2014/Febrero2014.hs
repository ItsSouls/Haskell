-------------------------------------------------------------------------------
-- Localización de las componentes fuertemente conexas de un digrafo 
--
-- Estructuras de Datos. Grado en Informática. UMA.
-- 18 de Febrero 2014
--
-- Apellidos:                  Nombre:  
-- Grado en Ingeniería ....
-- Grupo:               Puesto:
-------------------------------------------------------------------------------

module StronglyConnectedComponents  where

    import DataStructures.Graph.DiGraph 
    
    import DataStructures.Graph.DiGraphDFT
        ( dft         -- :: (Ord a) => DiGraph a -> a -> [a] 
        )
    import Data.List((\\), sort)
    
    -------
    -- A --
    -------
    reverseDiGraph :: Eq a => DiGraph a -> DiGraph a
    reverseDiGraph g = mkDiGraphEdges (vertices g) [y :-> x | x :-> y <- (diEdges g)]
                         
            
    {-gExample = mkDiGraphEdges [A .. H] 
                             [ A :-> B, B:-> F, B:->E, C:->D, C:->G, 
                               D:->C, D:->H, E:->F, E:->A,
                               F:->G, G:->F, H:->D, H:->G]
   
    gExample = mkDiGraphEdges [A .. H] 
                             [ B :-> A, F :-> B, E:->B, D:->C, C:->G, 
                               D:->C, D:->H, E:->F, E:->A,
                               F:->G, G:->F, H:->D, H:->G]
    -}-----
    -- B --
    -------
    restrictDiGraph :: Eq a => DiGraph a -> [a] -> DiGraph a
    -- el subgrafo de g con vértices en vs
    restrictDiGraph g [] = g
    restrictDiGraph g vs = mkDiGraphEdges vs [x :-> y | x <- vs, y <- successors g x, elem y vs]

    {-
    *StronglyConnectedComponents> restrictDiGraph gExample [A,B,E,F,G]
    Graph, Vertices : [A,B,H], DiEdges: [A :-> B]
    
    *StronglyConnectedComponents> reverseDiGraph $ restrictDiGraph gExample [A,B,H] 
    Graph, Vertices : [A,B,H], DiEdges: [B :-> A]
    -}
    
    type SCC a = [a] 
    
    -------
    -- C --
    -------
    sccOf :: Ord a => DiGraph a -> a -> SCC a
    -- la scc (strongly connected component) en el grafo g del vértice v
    sccOf g v = dft g' v
        where
            vs = dft g v
            gr = restrictDiGraph g vs
            g' = reverseDiGraph gr
    {-
    *StronglyConnectedComponents> sccOf  gExample A
    [A,E,B]
    -}
    
    -------
    -- D --
    -------
    -- todas las componentes
    sccs :: Ord a => DiGraph a -> [SCC a]
    sccs g = fil[sccOf g v | v <- (vertices g)] []
        where 
            fil [] aux = sort aux
            fil (x:xs) aux
                | elem (sort x) aux = fil xs aux
                | otherwise = fil xs (sort x:aux)  

    {-
    *StronglyConnectedComponents> sccs gExample
    [[A,E,B],[C,D,H],[F,G]]
    it :: [SCC Vertice]
    
    -- las componentes son tres ciclos
    -}
    
    
    -------------------------------------
    --- El grafo del enunciado del examen 
    -------------------------------------
    
    data Vertice = A|B|C|D|E|F|G|H|I|J|K deriving (Eq,Show,Ord,Enum)
    
    {-
     A ->B      C <---> D
     ^  /|      |       ^
     | / |      |       |
     |/. v      v       v   (atención al arco B :-> E
     E -> F<--> G <---- H
    -}
    gExample = mkDiGraphEdges [A .. H] 
                             [ A :-> B, B:-> F, B:->E, C:->D, C:->G, 
                               D:->C, D:->H, E:->F, E:->A,
                               F:->G, G:->F, H:->D, H:->G]
    
    g2 = sccs gExample



