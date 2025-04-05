-------------------------------------------------------------------------------
-- Apellidos, Nombre: 
-- Titulacion, Grupo: 
--
-- Estructuras de Datos. Grados en Informatica. UMA.
-------------------------------------------------------------------------------

module AVLBiDictionary( BiDictionary
                      , empty
                      , isEmpty
                      , size
                      , insert
                      , valueOf
                      , keyOf
                      , deleteByKey
                      , deleteByValue
                      , toBiDictionary
                      , compose
                      , isPermutation
                      , orbitOf
                      , cyclesOf
                      ) where

import qualified DataStructures.Dictionary.AVLDictionary as D
import qualified DataStructures.Set.BSTSet               as S

import           Data.List                               (intercalate, nub,
                                                          (\\), sort)
import           Data.Maybe                              (fromJust, fromMaybe,
                                                          isJust)
import           Test.QuickCheck


data BiDictionary a b = Bi (D.Dictionary a b) (D.Dictionary b a)

-- | Exercise a. empty, isEmpty, size

empty :: (Ord a, Ord b) => BiDictionary a b
empty = Bi D.empty D.empty

isEmpty :: (Ord a, Ord b) => BiDictionary a b -> Bool
isEmpty (Bi d1 d2) = if (D.isEmpty d1) && (D.isEmpty d2) then True else False

size :: (Ord a, Ord b) => BiDictionary a b -> Int
size (Bi d1 d2) = if D.isEmpty d1 then 0 else (D.size d1)

-- | Exercise b. insert

insert :: (Ord a, Ord b) => a -> b -> BiDictionary a b -> BiDictionary a b
insert a b (Bi d1 d2) 
  | D.isDefinedAt a d1 = Bi (D.insert a b (D.delete a d1)) (D.insert b a (D.delete (fromJust(D.valueOf a d1)) d2))
  | otherwise = Bi (D.insert a b d1) (D.insert b a d2)

-- | Exercise c. valueOf

valueOf :: (Ord a, Ord b) => a -> BiDictionary a b -> Maybe b
valueOf k (Bi d1 d2)
  | not (D.isDefinedAt k d1) = error "no existe la clave proporcionada"
  | otherwise = D.valueOf k d1

-- | Exercise d. keyOf

keyOf :: (Ord a, Ord b) => b -> BiDictionary a b -> Maybe a
keyOf v (Bi d1 d2)
  | not (D.isDefinedAt v d2) = error "no existe la clave proporcionada"
  | otherwise = D.valueOf v d2

-- | Exercise e. deleteByKey

deleteByKey :: (Ord a, Ord b) => a -> BiDictionary a b -> BiDictionary a b
deleteByKey k (Bi d1 d2)
  | not (D.isDefinedAt k d1) = (Bi d1 d2)
  | otherwise = Bi (D.delete k d1) (D.delete (fromJust (D.valueOf k d1)) d2)

-- | Exercise f. deleteByValue

deleteByValue :: (Ord a, Ord b) => b -> BiDictionary a b -> BiDictionary a b
deleteByValue v (Bi d1 d2)
  | not (D.isDefinedAt v d2) = (Bi d1 d2)
  | otherwise = Bi (D.delete (fromJust (D.valueOf v d2)) d1) (D.delete v d2) 


-- | Exercise g. toBiDictionary
isInyective :: (Eq b) => D.Dictionary a b -> Bool
isInyective d1 = isInyective' (D.values d1) []
  where
    isInyective' [] lista = True
    isInyective' (x:xs) lista
      | elem x lista = False
      | otherwise = isInyective' xs lista

toBiDictionary :: (Ord a, Ord b) => D.Dictionary a b -> BiDictionary a b
toBiDictionary dict
  | not (isInyective dict) = error "el diccionario no es inyectivo"
  | otherwise = (Bi dict (aux (D.keysValues dict) D.empty))
    where
      aux [] d2 = d2
      aux ((k,v):xs) d2 = aux xs (D.insert v k d2)
-- | Exercise h. compose

compose :: (Ord a, Ord b, Ord c) => BiDictionary a b -> BiDictionary b c -> BiDictionary a c
compose b1@(Bi b1d1 b1d2) b2@(Bi b2d1 b2d2) = toBiDictionary (aux (D.keysValues b1d1) b2d1 D.empty)
  where
    aux [] b2 daux = daux
    aux ((k1,v1):xs) b2 daux
      | D.isDefinedAt v1 b2 = aux xs b2 (D.insert k1 (fromJust(D.valueOf v1 b2)) daux)
      | otherwise = aux xs b2 daux

-- | Exercise i. isPermutation

isPermutation :: Ord a => BiDictionary a a -> Bool
isPermutation (Bi d1 d2) = if (sort (D.keys d1)) == (sort (D.keys d2)) then True else False



-- |------------------------------------------------------------------------


-- | Exercise j. orbitOf

orbitOf :: Ord a => a -> BiDictionary a a -> [a]
orbitOf = undefined

-- | Exercise k. cyclesOf

cyclesOf :: Ord a => BiDictionary a a -> [[a]]
cyclesOf = undefined

-- |------------------------------------------------------------------------


instance (Show a, Show b) => Show (BiDictionary a b) where
  show (Bi dk dv)  = "BiDictionary(" ++ intercalate "," (aux (D.keysValues dk)) ++ ")"
                        ++ "(" ++ intercalate "," (aux (D.keysValues dv)) ++ ")"
   where
    aux kvs  = map (\(k,v) -> show k ++ "->" ++ show v) kvs
